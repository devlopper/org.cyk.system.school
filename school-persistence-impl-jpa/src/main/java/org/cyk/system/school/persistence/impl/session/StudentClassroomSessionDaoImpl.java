package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.search.AbstractFieldValueSearchCriteriaSet;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.root.persistence.impl.QueryWrapper;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;

public class StudentClassroomSessionDaoImpl extends AbstractTypedDao<StudentClassroomSession> implements StudentClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readByClassroomSession,readByStudentByClassroomSession,readByAcademicSession,readByClassroomSessions,readByLevelTimeDivision;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSession, _select().where(StudentClassroomSession.FIELD_CLASSROOM_SESSION));
        registerNamedQuery(readByLevelTimeDivision, _select().where(commonUtils.attributePath(StudentClassroomSession.FIELD_CLASSROOM_SESSION, ClassroomSession.FIELD_LEVEL_TIME_DIVISION), ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
        registerNamedQuery(readByAcademicSession, _select().where(commonUtils.attributePath(StudentClassroomSession.FIELD_CLASSROOM_SESSION, ClassroomSession.FIELD_ACADEMIC_SESSION), ClassroomSession.FIELD_ACADEMIC_SESSION));
        registerNamedQuery(readByStudentByClassroomSession, _select().where(StudentClassroomSession.FIELD_STUDENT).and(StudentClassroomSession.FIELD_CLASSROOM_SESSION));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(StudentClassroomSession.FIELD_CLASSROOM_SESSION));
    
        registerNamedQuery(readByCriteria, "SELECT r FROM StudentClassroomSession r WHERE r.classroomSession.identifier IN :classroomSessions AND "
        		+ "EXISTS(SELECT r1 FROM StudentClassroomSessionDivision r1 WHERE r1.classroomSessionDivision.classroomSession = r.classroomSession AND "
        		+ "r1.student = r.student AND r1.classroomSessionDivision.globalIdentifier.orderNumber IN :classroomSessionDivisionIndexes AND "
        		+ "(SELECT COUNT(r2) FROM StudentClassroomSessionDivision r2 WHERE r2.classroomSessionDivision.classroomSession = r.classroomSession "
        		+ " AND r2.student = r.student ) >= :classroomSessionDivisionMinCount AND (SELECT COUNT(r3) FROM StudentClassroomSessionDivision r3 WHERE"
        		+ " r3.classroomSessionDivision.classroomSession = r.classroomSession AND r3.student = r.student ) <= :classroomSessionDivisionMaxCount)"
        		);
    }
    
	@Override
	public Collection<StudentClassroomSession> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession).resultMany();
	}

	@Override
	public StudentClassroomSession readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).
				parameter(StudentClassroomSession.FIELD_STUDENT, student).parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession)
				.ignoreThrowable(NoResultException.class)
				.resultOne();
	}

	@Override
	public Collection<StudentClassroomSession> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, levelTimeDivision).resultMany();
	}

	@Override
	protected void applySearchCriteriaParameters(QueryWrapper<?> queryWrapper,AbstractFieldValueSearchCriteriaSet searchCriteria) {
		super.applySearchCriteriaParameters(queryWrapper, searchCriteria);
		queryWrapper.parameter("classroomSessionDivisionIndexes",((StudentClassroomSession.SearchCriteria)searchCriteria).getDivisionIndexesRequired());
		queryWrapper.parameter("classroomSessionDivisionMinCount",((StudentClassroomSession.SearchCriteria)searchCriteria).getDivisionCount().getLowest());
		queryWrapper.parameter("classroomSessionDivisionMaxCount",((StudentClassroomSession.SearchCriteria)searchCriteria).getDivisionCount().getHighest());
		queryWrapper.parameterIdentifiers("classroomSessions",((StudentClassroomSession.SearchCriteria)searchCriteria).getClassroomSessions());
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<StudentClassroomSession> readByCriteria(SearchCriteria searchCriteria) {
		String queryName = readByCriteria;
		QueryWrapper<?> queryWrapper = namedQuery(queryName);
		applySearchCriteriaParameters(queryWrapper, searchCriteria);
		return (Collection<StudentClassroomSession>) queryWrapper.resultMany();
	}

	@Override
	public Long countByCriteria(SearchCriteria searchCriteria) {
		QueryWrapper<?> queryWrapper = countNamedQuery(countByCriteria);
		applySearchCriteriaParameters(queryWrapper, searchCriteria);
		return (Long) queryWrapper.resultOne();
	}

	@Override
	public Collection<StudentClassroomSession> readByAcademicSession(AcademicSession academicSession) {
		return namedQuery(readByAcademicSession).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession).resultMany();
	}

}
 