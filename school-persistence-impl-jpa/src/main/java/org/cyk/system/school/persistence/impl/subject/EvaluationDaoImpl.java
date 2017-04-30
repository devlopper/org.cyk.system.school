package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;

public class EvaluationDaoImpl extends AbstractTypedDao<Evaluation> implements EvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubject,countByClassroomSessionDivisionSubject,
		readByClassroomSessionDivisionSubjectEvaluationTypes,countByClassroomSessionDivisionSubjectEvaluationTypes
		//,readByAcademicSessionByLevelByTimeDivisionTypeByClassroomSessionSuffixByDivisionOrderNumberBySubjectByEvaluationType
		;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(commonUtils.attributePath(Evaluation.FIELD_TYPE
				, ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT), ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
		registerNamedQuery(readByClassroomSessionDivisionSubjectEvaluationTypes, _select().whereIdentifierIn(Evaluation.FIELD_TYPE));
		/*
		registerNamedQuery(readByAcademicSessionByLevelByTimeDivisionTypeByClassroomSessionSuffixByDivisionOrderNumberBySubjectByEvaluationType, _select()
			.where(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
					,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION,ClassroomSession.FIELD_ACADEMIC_SESSION)
					,ClassroomSession.FIELD_ACADEMIC_SESSION)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
					,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
					, ClassroomSessionDivision.FIELD_CLASSROOMSESSION, ArithmeticOperator.EQ)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
					, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, ArithmeticOperator.EQ)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT)
					, ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ArithmeticOperator.EQ)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
					,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
					, "", ArithmeticOperator.EQ)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
					,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
					, "", ArithmeticOperator.EQ)
			.and(commonUtils.attributePath(Evaluation.FIELD_TYPE,ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
					,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
					, "", ArithmeticOperator.EQ)
			);*/
	}
	
	@Override
	public Collection<Evaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject).resultMany();
	}
	
	@Override
	public Long countByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return countNamedQuery(countByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject).resultOne();
	}

	@Override
	public Collection<Evaluation> readByClassroomSessionDivisionSubjectEvaluationTypes(Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes) {
		return namedQuery(readByClassroomSessionDivisionSubjectEvaluationTypes).parameterIdentifiers(classroomSessionDivisionSubjectEvaluationTypes).resultMany();
	}
	
	@Override
	public Collection<Evaluation> readByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		return readByClassroomSessionDivisionSubjectEvaluationTypes(Arrays.asList(classroomSessionDivisionSubjectEvaluationType));
	}
	
	@Override
	public Long countByClassroomSessionDivisionSubjectEvaluationTypes(Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes) {
		return countNamedQuery(countByClassroomSessionDivisionSubjectEvaluationTypes).parameterIdentifiers(classroomSessionDivisionSubjectEvaluationTypes).resultOne();
	}
	
	@Override
	public Long countByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		return countByClassroomSessionDivisionSubjectEvaluationTypes(Arrays.asList(classroomSessionDivisionSubjectEvaluationType));
	}

	@Override
	public Collection<Evaluation> readByAcademicSessionByLevelByTimeDivisionTypeByClassroomSessionSuffixByDivisionOrderNumberBySubjectByEvaluationType(AcademicSession academicSession,Level level, TimeDivisionType timeDivisionType,ClassroomSessionSuffix classroomSessionSuffix,Long classroomSessionDivisionOrderNumber, Subject subject,
			EvaluationType evaluationType) {
		
		return null;
	}
	
	
	
}
 