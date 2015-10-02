package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionDivisionSubjectBusinessImpl(ClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}

	

    
}
