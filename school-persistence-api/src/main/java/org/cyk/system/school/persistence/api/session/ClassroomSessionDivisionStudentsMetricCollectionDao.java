package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;

public interface ClassroomSessionDivisionStudentsMetricCollectionDao extends TypedDao<ClassroomSessionDivisionStudentsMetricCollection> {
	
	Collection<ClassroomSessionDivisionStudentsMetricCollection> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

}
