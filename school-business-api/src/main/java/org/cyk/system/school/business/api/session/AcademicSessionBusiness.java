package org.cyk.system.school.business.api.session;

import org.cyk.system.root.business.api.event.AbstractIdentifiablePeriodBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.School;

public interface AcademicSessionBusiness extends AbstractIdentifiablePeriodBusiness<AcademicSession> {

	AcademicSession findCurrent(School school);
	
}
