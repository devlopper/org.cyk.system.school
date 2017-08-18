package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionSubjectDao;
import org.cyk.utility.common.helper.FieldHelper;

public class StudentClassroomSessionSubjectDaoImpl extends AbstractTypedDao<StudentClassroomSessionSubject> /*AbstractStudentResultsDaoImpl<ClassroomSessionDivisionSubject,StudentSubject,StudentSubjectEvaluation>*/ implements StudentClassroomSessionSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByStudentByClassroomSession,readByClassroomSessionSubject;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByStudentByClassroomSession, _select().where(FieldHelper.getInstance()
			.buildPath(StudentClassroomSessionSubject.FIELD_CLASSROOM_SESSION_SUBJECT,ClassroomSessionSubject.COLUMN_CLASSROOMSESSION)
			,ClassroomSessionSubject.COLUMN_CLASSROOMSESSION).and(StudentClassroomSessionSubject.FIELD_STUDENT));
		
		registerNamedQuery(readByClassroomSessionSubject, _select().where(StudentClassroomSessionSubject.FIELD_CLASSROOM_SESSION_SUBJECT));
	}
	
	@Override
	public Collection<StudentClassroomSessionSubject> readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).parameter(StudentClassroomSessionSubject.FIELD_STUDENT, student)
				.parameter(ClassroomSessionSubject.COLUMN_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionSubject> readByStudentClassroomSession(StudentClassroomSession studentClassroomSession) {
		return readByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession());
	}

	@Override
	public Collection<StudentClassroomSessionSubject> readByClassroomSessionSubject(ClassroomSessionSubject classroomSessionSubject) {
		return namedQuery(readByClassroomSessionSubject).parameter(StudentClassroomSessionSubject.FIELD_CLASSROOM_SESSION_SUBJECT, classroomSessionSubject).resultMany();
	}
	
    
}
 