package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.subject.SubjectClassroomSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.business.impl.subject.SubjectClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private SubjectClassroomSessionBusiness subjectClassroomSessionBusiness;
	
	private FormOneData<ClassroomSessionDetails> details;
	private Table<ClassroomSessionDivisionDetails> divisionTable;
	private Table<SubjectClassroomSessionDetails> subjectTable;
	private Table<StudentClassroomSessionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = formatPathUsingBusiness(ClassroomSession.class,identifiable);
		
		details = createDetailsForm(ClassroomSessionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<ClassroomSession,ClassroomSessionDetails>(ClassroomSession.class, ClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		divisionTable = (Table<ClassroomSessionDivisionDetails>) createDetailsTable(ClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails>(ClassroomSessionDivision.class, ClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivision> getIdentifiables() {
				return identifiable.getDivisions();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		subjectTable = (Table<SubjectClassroomSessionDetails>) createDetailsTable(SubjectClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<SubjectClassroomSession,SubjectClassroomSessionDetails>(SubjectClassroomSession.class, SubjectClassroomSessionDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public Collection<SubjectClassroomSession> getIdentifiables() {
					return subjectClassroomSessionBusiness.findByClassroomSession(identifiable);
				}
				@Override
				public Crud[] getCruds() {
					return new Crud[]{Crud.READ,Crud.UPDATE};
				}
			});
		
		studentTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>(StudentClassroomSession.class, StudentClassroomSessionDetails.class){
				private static final long serialVersionUID = 1L;
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
		contextualMenu.setLabel(formatUsingBusiness(identifiable)); 
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		for(ClassroomSessionDivision classroomSessionDivision : identifiable.getDivisions()){
			contextualMenu.getChildren().add(navigationManager.createConsultCommandable(classroomSessionDivision,null));
		}
		return Arrays.asList(contextualMenu);
	}
	
	/**/

}
