package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionSubjectDetails;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.ui.api.IdentifierProvider;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultPage extends AbstractConsultPage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentClassroomSessionSubjectDetails> subjectTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		subjectTable = (Table<StudentClassroomSessionSubjectDetails>) createDetailsTable(StudentClassroomSessionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionSubject,StudentClassroomSessionSubjectDetails>(StudentClassroomSessionSubject.class, StudentClassroomSessionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionSubject> getIdentifiables() {
				return inject(StudentClassroomSessionSubjectBusiness.class).findByStudentClassroomSession(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(StudentClassroomSession.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "studentClassroomSessionEditSubjectsView";
			}
			
			@Override
			public AbstractIdentifiable getFormIdentifiable() {
				return identifiable;
			}
			
		});
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		((Commandable)subjectTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
	}

}
