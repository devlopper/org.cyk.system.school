package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.utility.common.Constant;

public class ClassroomSessionBusinessImpl extends AbstractTypedBusinessService<ClassroomSession, ClassroomSessionDao> implements ClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public ClassroomSessionBusinessImpl(ClassroomSessionDao dao) {
		super(dao);  
	}
	
	@Override
	public Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession) {
		return dao.readByAcademicSession(academicSession);
	}

	@Override
	public String format(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getName().getName()
				+(StringUtils.isBlank(classroomSession.getSuffix())?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE+classroomSession.getSuffix());
	}
	
	@Override
	protected void __load__(ClassroomSession classroomSession) {
		super.__load__(classroomSession);
		classroomSession.setDivisions(classroomSessionDivisionDao.readByClassroomSession(classroomSession));
	}

}
