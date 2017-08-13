package org.cyk.system.school.business.api.session;

import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.School;

public interface CommonNodeInformationsBusiness {

	<T> T findValue(School school,Class<T> aClass,String commonNodeInformationsFieldName);
	<T> T findValue(AcademicSession academicSession,Class<T> aClass,String commonNodeInformationsFieldName);
	<T> T findValue(LevelName levelName,Class<T> aClass,String commonNodeInformationsFieldName);
	<T> T findValue(ClassroomSession classroomSession,Class<T> aClass,String commonNodeInformationsFieldName);
	
}
