package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.subject.SubjectClassroomSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
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
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable);
		
		details = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(details,new DetailsFormOneDataConfigurationAdapter<ClassroomSession,Details>(ClassroomSession.class, Details.class){
			private static final long serialVersionUID = 1L;
			@Override
			public String getTitleId() {
				return "model.entity.classroomSession";
			}
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		divisionTable = (Table<DivisionDetails>) createDetailsTable(DivisionDetails.class, new DetailsTableConfigurationAdapter<ClassroomSessionDivision,DivisionDetails>(ClassroomSessionDivision.class, DivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public String getTitleId() {
				return "model.entity.classroomSessionDivision";
			}
			@Override
			public Collection<ClassroomSessionDivision> getIdentifiables() {
				return identifiable.getDivisions();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		subjectTable = (Table<SubjectDetails>) createDetailsTable(SubjectDetails.class, new DetailsTableConfigurationAdapter<SubjectClassroomSession,SubjectDetails>(SubjectClassroomSession.class, SubjectDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public String getTitleId() {
					return "model.entity.subject";
				}
				@Override
				public Collection<SubjectClassroomSession> getIdentifiables() {
					return subjectClassroomSessionBusiness.findByClassroomSession(identifiable);
				}
				@Override
				public Crud[] getCruds() {
					return new Crud[]{Crud.READ,Crud.UPDATE};
				}
			});
		
		studentTable = (Table<StudentDetails>) createDetailsTable(StudentDetails.class, new DetailsTableConfigurationAdapter<StudentClassroomSession,StudentDetails>(StudentClassroomSession.class, StudentDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public String getTitleId() {
					return "model.entity.student";
				}
				@Override
				public Collection<StudentClassroomSession> getIdentifiables() {
					return SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().findByClassroomSession(identifiable);
				}
				@Override
				public Crud[] getCruds() {
					return new Crud[]{Crud.CREATE,Crud.READ,Crud.UPDATE,Crud.DELETE};
				}
			});
					
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
		@Input @InputText private String name;
		public SubjectDetails(SubjectClassroomSession subjectClassroomSession) {
			super(subjectClassroomSession);
			name = subjectClassroomSession.getSubject().getName();
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
