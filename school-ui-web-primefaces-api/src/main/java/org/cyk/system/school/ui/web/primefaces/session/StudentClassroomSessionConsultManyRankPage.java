package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.SortByRankArguments;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Rank;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultManyRankPage extends AbstractConsultPage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentClassroomSessionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		Collection<Long> identifiers = WebManager.getInstance().decodeIdentifiersRequestParameter();
		final Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(AbstractIdentifiable identifiable : RootBusinessLayer.getInstance().getGenericBusiness()
				.use((Class<? extends AbstractIdentifiable>) StudentClassroomSession.class).findByIdentifiers(identifiers))
			studentClassroomSessions.add((StudentClassroomSession) identifiable);
		
		studentTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new TableAdapter(){
			private static final long serialVersionUID = -5840897096825258754L;

			@Override
			protected Collection<StudentClassroomSession> getStudentClassroomSessions() {
				return studentClassroomSessions;
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
		});
		
		
		Collection<ClassroomSession> classroomSessions = new ArrayList<>();
		for(StudentClassroomSession studentClassroomSession : studentClassroomSessions)
			classroomSessions.add(studentClassroomSession.getClassroomSession());
		
		final List<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(filterClassroomSessionDivisions(SchoolBusinessLayer.getInstance()
				.getClassroomSessionDivisionBusiness().findByClassroomSessions(classroomSessions)));
		
		studentTable.getColumnListeners().add(new ColumnAdapter(userSession, classroomSessionDivisions));
		
		studentTable.getCellListeners().add(new CellAdpater(classroomSessionDivisions) {
			private static final long serialVersionUID = 1L;
			@Override
			protected Collection<StudentClassroomSessionDivision> getDetailCollection() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisions(classroomSessionDivisions);
			}
		});

	}
	
	@Override
	protected <T extends AbstractIdentifiable> T identifiableFromRequestParameter(Class<T> aClass) {
		return null;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		return null;
	}
	
	public static Collection<ClassroomSessionDivision> filterClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions){
		Collection<ClassroomSessionDivision> result = new ArrayList<>();
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			Boolean found = Boolean.FALSE;
			for(ClassroomSessionDivision index : result)
				if(index.getTimeDivisionType().equals(classroomSessionDivision.getTimeDivisionType()) && index.getIndex()==classroomSessionDivision.getIndex()){
					found = Boolean.TRUE;
					break;
				}
			if(Boolean.FALSE.equals(found))
				result.add(classroomSessionDivision);
		}
		return result;
	}
	
	public static abstract class TableAdapter extends DetailsConfigurationListener.Table.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>{

		private static final long serialVersionUID = 1490623689747277860L;

		public TableAdapter() {
			super(StudentClassroomSession.class, StudentClassroomSessionDetails.class);
		}
		
		protected abstract Collection<StudentClassroomSession> getStudentClassroomSessions();
		
		@Override
		public Collection<StudentClassroomSession> getIdentifiables() {
			Collection<StudentClassroomSession> collection = getStudentClassroomSessions();
			final List<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
			if(collection!=null)
				studentClassroomSessions.addAll(collection);
			RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
	        rankOptions.setType(RankType.EXAEQUO); 
	        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
			SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().computeRank(studentClassroomSessions, rankOptions);
			RootBusinessLayer.getInstance().getMathematicsBusiness().sortByRank(new SortByRankArguments<StudentClassroomSession>() {
				@Override
				public Rank getRank(StudentClassroomSession studentClassroomSession) {
					return studentClassroomSession.getResults().getEvaluationSort().getRank();
				}
				
				@Override
				public List<StudentClassroomSession> getObjects() {
					return studentClassroomSessions;
				}
			});
			return studentClassroomSessions;
		}
		
	}

	@Getter @Setter
	public static class ColumnAdapter extends org.cyk.system.school.ui.web.primefaces.session.AbstractClassLevelConsultPage.ColumnAdapter<ClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = 5857490517889645397L;
		
		public ColumnAdapter(AbstractUserSession<?, ?> userSession,List<ClassroomSessionDivision> subLevels) {
			super(userSession, null, null, ClassroomSessionDivision.class, subLevels, 2, StudentClassroomSessionDetails.FIELDS_BROAD_SHEET);
		}
	}
	
	public static abstract class CellAdpater extends org.cyk.system.school.ui.web.primefaces.session.AbstractClassLevelConsultPage.CellAdapter<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDetails> {
		private static final long serialVersionUID = 2113891697444367237L;
		
		public CellAdpater(List<ClassroomSessionDivision> subLevels) {
			super(2, subLevels);
		}

		@Override
		protected ClassroomSessionDivision getSubLevel(StudentClassroomSessionDivision studentClassroomSessionDivision) {
			for(ClassroomSessionDivision classroomSessionDivision : subLevels)
				if(studentClassroomSessionDivision.getClassroomSessionDivision().getTimeDivisionType().equals(classroomSessionDivision.getTimeDivisionType()) 
						&& studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()==classroomSessionDivision.getIndex())
					return classroomSessionDivision;
			return null;
		}
		
		@Override
		protected NodeResults getNodeResults(ClassroomSessionDivision classroomSessionDivision) {
			return classroomSessionDivision.getResults();
		}
		
	}
	
}
