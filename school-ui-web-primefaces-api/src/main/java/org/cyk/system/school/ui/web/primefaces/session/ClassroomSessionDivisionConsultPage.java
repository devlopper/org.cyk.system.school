package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.api.IdentifierProvider;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionDivisionSubjectDetails> subjectTable;
	private Table<StudentClassroomSessionDivisionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		subjectTable = (Table<ClassroomSessionDivisionSubjectDetails>) createDetailsTable(ClassroomSessionDivisionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivisionSubject,ClassroomSessionDivisionSubjectDetails>(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivisionSubject> getIdentifiables() {
				return inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivision(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(ClassroomSessionDivision.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "classroomSessionDivisionEditSubjectsView";
			}
			
			@Override
			public AbstractIdentifiable getFormIdentifiable() {
				return identifiable;
			}
			
		});
		
		studentTable = (Table<StudentClassroomSessionDivisionDetails>) createDetailsTable(StudentClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails>(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivision> getIdentifiables() {
				return inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSessionDivision(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(ClassroomSessionDivision.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "classroomSessionDivisionEditStudentsView";
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
		((Commandable)studentTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
		((Commandable)subjectTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
	}
	
	@Override
	protected String getContentTitleIdentifiableText() {
		return formatUsingBusiness(new Object[]{identifiable.getClassroomSession(),identifiable});
	}
	
	
}
