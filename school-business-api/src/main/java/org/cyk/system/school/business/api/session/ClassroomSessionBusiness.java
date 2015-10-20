package org.cyk.system.school.business.api.session;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.ClassroomSession;

public interface ClassroomSessionBusiness extends TypedBusiness<ClassroomSession> {

	String format(ClassroomSession classroomSession);
	
}
