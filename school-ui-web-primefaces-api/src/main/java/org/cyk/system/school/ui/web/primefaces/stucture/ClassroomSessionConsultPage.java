package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.ui.web.primefaces.CompanyWebManager;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.subject.SubjectClassroomSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.DefaultMenu;
import org.cyk.ui.api.command.menu.UIMenu.RenderType;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private SubjectClassroomSessionBusiness subjectClassroomSessionBusiness;
	
	private FormOneData<Details> details;
	private Table<DivisionDetails> divisionTable;
	private Table<SubjectDetails> subjectTable;
	private Table<StudentDetails> studentTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable);
		
		details = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(details);
		
		divisionTable = (Table<DivisionDetails>) createTable(DivisionDetails.class, null, null);
		configureDetailsTable(divisionTable, "model.entity.classroomSessionDivision",new Crud[]{Crud.READ,Crud.UPDATE});
		for(ClassroomSessionDivision classroomSessionDivision : identifiable.getDivisions())
			divisionTable.getInitialData().add(new DivisionDetails(classroomSessionDivision));
		
		subjectTable = (Table<SubjectDetails>) createTable(SubjectDetails.class, null, null);
		configureDetailsTable(subjectTable, "model.entity.subject");
		for(SubjectClassroomSession subjectClassroomSession : subjectClassroomSessionBusiness.findByClassroomSession(identifiable))
			subjectTable.getInitialData().add(new SubjectDetails(subjectClassroomSession));
		
		studentTable = (Table<StudentDetails>) createTable(StudentDetails.class, null, null);
		configureDetailsTable(studentTable, "model.entity.subject");
		for(StudentClassroomSession studentClassroomSession : SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().findByClassroomSession(identifiable))
			studentTable.getInitialData().add(new StudentDetails(studentClassroomSession));
		
		detailsMenu = new DefaultMenu();
		detailsMenu.addCommandable("t1", null, CompanyWebManager.getInstance().getOutcomeCustomerBalance());
		detailsMenu.addCommandable("t2", null, CompanyWebManager.getInstance().getOutcomeCustomerSaleStock());
		detailsMenu.addCommandable("t3", null, CompanyWebManager.getInstance().getOutcomeEditSaleDeliveryDetails());
		
		
	}
	
	@Override
	protected void afterInitialisation() {
		// TODO Auto-generated method stub
		super.afterInitialisation();
		
	}
	
	@Override
	public void targetDependentInitialisation() {
		// TODO Auto-generated method stub
		super.targetDependentInitialisation();
		//detailsMenu.setRenderType(RenderType.PLAIN);
	}
		
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		for(ClassroomSessionDivision classroomSessionDivision : identifiable.getDivisions()){
			commandable = navigationManager.createConsultCommandable(classroomSessionDivision,"button",null);
			commandable.setLabel(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(classroomSessionDivision));
			contextualMenu.getChildren().add(commandable);
		}
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String name,coordinator;
		public Details(ClassroomSession classroomSession) {
			super(classroomSession);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(classroomSession);
			coordinator = classroomSession.getCoordinator().getPerson().getNames();
		}
	}
	
	public static class DivisionDetails extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String name,duration;
		public DivisionDetails(ClassroomSessionDivision classroomSessionDivision) {
			super(classroomSessionDivision);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(classroomSessionDivision);
			duration = timeBusiness.formatDuration(classroomSessionDivision.getDuration());
		}
	}
	
	public static class SubjectDetails extends AbstractOutputDetails<SubjectClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String names;
		public SubjectDetails(SubjectClassroomSession subjectClassroomSession) {
			super(subjectClassroomSession);
			names = subjectClassroomSession.getSubject().getName();
		}
	}
	
	public static class StudentDetails extends AbstractOutputDetails<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String names;
		public StudentDetails(StudentClassroomSession studentClassroomSession) {
			super(studentClassroomSession);
			names = studentClassroomSession.getStudent().getPerson().getNames();
		}
	}

}
