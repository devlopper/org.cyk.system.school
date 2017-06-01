package org.cyk.system.school.persistence.api.session;

import org.cyk.system.root.persistence.api.event.AbstractIdentifiablePeriodDao;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.School;

public interface AcademicSessionDao extends AbstractIdentifiablePeriodDao<AcademicSession> {

	AcademicSession readDefaultedBySchool(School school);

}
