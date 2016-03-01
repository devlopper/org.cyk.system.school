package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionStudentsMetricCollectionBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionStudentsMetricCollectionDao;

public class ClassroomSessionDivisionStudentsMetricCollectionBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionStudentsMetricCollection, ClassroomSessionDivisionStudentsMetricCollectionDao> implements ClassroomSessionDivisionStudentsMetricCollectionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	
	@Inject
	public ClassroomSessionDivisionStudentsMetricCollectionBusinessImpl(ClassroomSessionDivisionStudentsMetricCollectionDao dao) {
		super(dao);  
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionStudentsMetricCollection> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}

}
