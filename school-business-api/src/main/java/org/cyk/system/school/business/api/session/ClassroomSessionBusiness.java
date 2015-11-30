package org.cyk.system.school.business.api.session;

import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;

public interface ClassroomSessionBusiness extends TypedBusiness<ClassroomSession> {

	String format(ClassroomSession classroomSession);

	Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession);
	
	CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession);
	
	BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond);
	Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration);
	
}
