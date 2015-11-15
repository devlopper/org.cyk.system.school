package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;

public interface ClassroomSessionDao extends TypedDao<ClassroomSession> {

	Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession);


}
