package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionStudentsMetricCollectionDao;

public class ClassroomSessionDivisionStudentsMetricCollectionDaoImpl extends AbstractTypedDao<ClassroomSessionDivisionStudentsMetricCollection> implements ClassroomSessionDivisionStudentsMetricCollectionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivision;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivision, _select().where(ClassroomSessionDivisionStudentsMetricCollection.FIELD_CLASSROOMSESSIONDIVISION));
	}

	@Override
	public Collection<ClassroomSessionDivisionStudentsMetricCollection> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionStudentsMetricCollection.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision).resultMany();
	}
	
}
 