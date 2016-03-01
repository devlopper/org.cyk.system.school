package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;

public interface ClassroomSessionDivisionStudentsMetricCollectionBusiness extends TypedBusiness<ClassroomSessionDivisionStudentsMetricCollection> {

	Collection<ClassroomSessionDivisionStudentsMetricCollection> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	
}
