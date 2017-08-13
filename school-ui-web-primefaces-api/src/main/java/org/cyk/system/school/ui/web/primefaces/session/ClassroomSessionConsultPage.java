package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionSubjectDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.IdentifierProvider;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionDivisionDetails> divisionTable;
	private Table<ClassroomSessionSubjectDetails> subjectTable;
	private Table<StudentClassroomSessionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		divisionTable = (Table<ClassroomSessionDivisionDetails>) createDetailsTable(ClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails>(ClassroomSessionDivision.class, ClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivision> getIdentifiables() {
				return inject(ClassroomSessionDivisionBusiness.class).findByClassroomSession(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(ClassroomSession.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "classroomSessionEditDivisionsView";
			}
			
			@Override
			public AbstractIdentifiable getFormIdentifiable() {
				return identifiable;
			}
			
		});
		
		subjectTable = (Table<ClassroomSessionSubjectDetails>) createDetailsTable(ClassroomSessionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionSubject,ClassroomSessionSubjectDetails>(ClassroomSessionSubject.class, ClassroomSessionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionSubject> getIdentifiables() {
				return inject(ClassroomSessionSubjectBusiness.class).findByClassroomSession(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(ClassroomSession.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "classroomSessionEditSubjectsView";
			}
			
			@Override
			public AbstractIdentifiable getFormIdentifiable() {
				return identifiable;
			}
			
		});
		
		studentTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>(StudentClassroomSession.class, StudentClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSession> getIdentifiables() {
				return inject(StudentClassroomSessionBusiness.class).findByClassroomSession(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(ClassroomSession.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "classroomSessionEditStudentsView";
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
		((Commandable)divisionTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
		((Commandable)studentTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
		((Commandable)subjectTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
	}

	
	
}
