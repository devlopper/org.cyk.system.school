package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionSubjectDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.IdentifierProvider;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.api.model.table.RowAdapter;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.Table;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPageOLD extends AbstractClassLevelConsultPage<ClassroomSession,ClassroomSessionDetails,ClassroomSessionDivision,ClassroomSessionDivisionDetails,StudentClassroomSession,StudentClassroomSessionDetails,StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionSubjectDetails> subjectTable;
	private String currentClassroomSessionDivisionMessage;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		subLevelTable.getRowListeners().add(new RowAdapter<ClassroomSessionDivisionDetails>(userSession){
			private static final long serialVersionUID = 1L;
			@Override
			public void added(Row<ClassroomSessionDivisionDetails> row) {
				super.added(row);
				row.setHtml(row.getData().getMaster().getTimeDivisionType().getName());
			}
		});
		
		subjectTable.getRowListeners().add(new RowAdapter<ClassroomSessionSubjectDetails>(userSession){
			private static final long serialVersionUID = 1L;
			@Override
			public void added(Row<ClassroomSessionSubjectDetails> row) {
				super.added(row);
				//row.setHtml(row.getData().getMaster().getSubject().getName()+" / "+row.getData().getMaster().getTeacher().getPerson().getNames());
			}
		});
		
		//inject(LanguageBusiness.class).findText("current.classroomsessiondivision", new Object[]{identifiable.getNodeInformations().getCurrentClassroomSessionDivisionIndex()});
		//currentClassroomSessionDivisionMessage = StringHelper.getInstance().get("current.classroomsessiondivision", new Object[]{identifiable.getNodeInformations().getCurrentClassroomSessionDivisionIndex()});
		
		currentClassroomSessionDivisionMessage = inject(LanguageBusiness.class).findText("current.classroomsessiondivision", new Object[]{identifiable
				.getAcademicSession().getNodeInformations().getCurrentClassroomSessionDivisionIndex()});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		((Commandable)subLevelTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
		((Commandable)studentTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
		((Commandable)subjectTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
	}

	@Override
	protected void subLevelTable() {
		super.subLevelTable();
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
	}
	
	protected DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails> getSubLevelTableCreationListener(){
		return new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails>(getSubLevelClass(), getSubLevelOutputClass()){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivision> getIdentifiables() {
				return getSubLevels();
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
		};
	}
	
	@Override
	protected Class<ClassroomSession> getLevelClass() {
		return ClassroomSession.class;
	}

	@Override
	protected Class<ClassroomSessionDetails> getLevelOutputClass() {
		return ClassroomSessionDetails.class;
	}
	
	@Override
	protected Class<ClassroomSessionDivision> getSubLevelClass() {
		return ClassroomSessionDivision.class;
	}

	@Override
	protected Class<ClassroomSessionDivisionDetails> getSubLevelOutputClass() {
		return ClassroomSessionDivisionDetails.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivision> getDetailClass() {
		return StudentClassroomSessionDivision.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivisionDetails> getDetailOutputClass() {
		return StudentClassroomSessionDivisionDetails.class;
	}

	@Override
	protected Class<StudentClassroomSession> getResultClass() {
		return StudentClassroomSession.class;
	}

	@Override
	protected Class<StudentClassroomSessionDetails> getResultOutputClass() {
		return StudentClassroomSessionDetails.class;
	}

	@Override
	protected ClassroomSession getClassroomSession() {
		return identifiable;
	}

	@Override
	protected Collection<StudentClassroomSession> getResults() {
		return inject(StudentClassroomSessionBusiness.class).findByClassroomSession(identifiable);
	}
	
	@Override
	protected Collection<ClassroomSessionDivision> getSubLevels() {
		if(Boolean.TRUE.equals(userSession.getIsManager()) || isCoordinator)
			return inject(ClassroomSessionDivisionBusiness.class).findByClassroomSession(identifiable);
		else
			if(teacher==null)
				return null;
			else
				return inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByTeacher(identifiable,teacher);
	}
	
	@Override
	protected Set<String> getResultTableSimpleFieldNameSet() {
		return StudentClassroomSessionDetails.FIELDS_SIMPLE;
	}

	@Override
	protected Set<String> getResultTableBroadsheetFieldNameSet() {
		return StudentClassroomSessionDetails.FIELDS_BROAD_SHEET;
	}

	@Override
	protected CellAdapter<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDetails> getBroadsheetTableCellAdapter(List<ClassroomSessionDivision> classroomSessionDivisions) {
		return new CellAdapter<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDetails>(2,classroomSessionDivisions) {
			private static final long serialVersionUID = 2113891697444367237L;

			@Override
			protected ClassroomSessionDivision getSubLevel(StudentClassroomSessionDivision studentClassroomSessionDivision) {
				return studentClassroomSessionDivision.getClassroomSessionDivision();
			}
			
			@Override
			protected NodeResults getNodeResults(ClassroomSessionDivision classroomSessionDivision) {
				return classroomSessionDivision.getResults();
			}
			
			@Override
			protected Collection<StudentClassroomSessionDivision> getDetailCollection() {
				if(Boolean.TRUE.equals(userSession.getIsManager()) || isCoordinator)
					return inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSession(identifiable);
				else
					if(teacher==null)
						return null;
					else
						return inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSessionByTeacher(identifiable,teacher);
			}
		};
	}

}