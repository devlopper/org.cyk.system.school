package org.cyk.system.school.business.api.session;

import java.math.BigDecimal;

import org.cyk.system.root.business.api.event.AbstractIdentifiablePeriodBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.School;

public interface AcademicSessionBusiness extends AbstractIdentifiablePeriodBusiness<AcademicSession> {

	AcademicSession findCurrent(School school);

	BigDecimal convertAttendanceTimeToDivisionDuration(Long millisecond);
	Long convertAttendanceTimeToMillisecond(BigDecimal duration);
	
	AcademicSession update(AcademicSession academicSession,Boolean cascade);
}
