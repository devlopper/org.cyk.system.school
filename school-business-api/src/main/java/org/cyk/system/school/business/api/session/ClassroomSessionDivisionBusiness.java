package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;

public interface ClassroomSessionDivisionBusiness extends TypedBusiness<ClassroomSessionDivision> {

	// should be moved to a super interface
	void computeResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);

	Collection<ClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession);
	
	String format(ClassroomSessionDivision classroomSessionDivision);
	
	Integer findIndex(ClassroomSessionDivision classroomSessionDivision);
	
}
