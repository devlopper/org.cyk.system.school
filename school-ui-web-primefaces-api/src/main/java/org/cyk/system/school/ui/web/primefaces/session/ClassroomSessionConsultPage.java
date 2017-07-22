package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionSubjectDetails;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.ui.web.primefaces.Table;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractClassLevelConsultPage<ClassroomSession,ClassroomSessionDetails,ClassroomSessionDivision,ClassroomSessionDivisionDetails,StudentClassroomSession,StudentClassroomSessionDetails,StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionSubjectDetails> subjectTable;
		
	@Override
	protected void subLevelTable() {
		super.subLevelTable();
		subjectTable = (Table<ClassroomSessionSubjectDetails>) createDetailsTable(ClassroomSessionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionSubject,ClassroomSessionSubjectDetails>(ClassroomSessionSubject.class, ClassroomSessionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionSubject> getIdentifiables() {
				return inject(ClassroomSessionSubjectBusiness.class).findByClassroomSession(identifiable);
			}
		});
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
