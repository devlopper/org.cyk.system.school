package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;

public class ClassroomSessionBusinessImpl extends AbstractTypedBusinessService<ClassroomSession, ClassroomSessionDao> implements ClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public ClassroomSessionBusinessImpl(ClassroomSessionDao dao) {
		super(dao);  
	}

	@Override
	public String format(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getName().getName();
	}
	
	@Override
	protected void __load__(ClassroomSession classroomSession) {
		super.__load__(classroomSession);
		classroomSession.setDivisions(classroomSessionDivisionDao.readByClassroomSession(classroomSession));
	}

}
