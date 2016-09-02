package org.cyk.system.school.business.api.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

public interface StudentClassroomSessionBusiness extends AbstractStudentResultsBusiness<ClassroomSession,StudentClassroomSession,StudentClassroomSessionDivision> {

	Collection<StudentClassroomSession> findByClassroomSession(ClassroomSession classroomSession);
	Collection<StudentClassroomSession> findByClassroomSessions(Collection<ClassroomSession> classroomSessions);
	Collection<StudentClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	StudentClassroomSession findByStudentByClassroomSession(Student student, ClassroomSession classroomSession);
	
	Collection<StudentClassroomSession> findByCriteria(SearchCriteria criteria);
	Long countByCriteria(SearchCriteria criteria);
	
	/**/
	
	@Getter @Setter @NoArgsConstructor
	public static class ServiceCallArguments extends BusinessServiceCallArguments<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = 7151479991050865862L;
		
	}

	Collection<StudentClassroomSession> findByAcademicSession(AcademicSession academicSession);

	
}
