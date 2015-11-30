package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.utility.common.Constant;

@Stateless
public class ClassroomSessionBusinessImpl extends AbstractTypedBusinessService<ClassroomSession, ClassroomSessionDao> implements ClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public ClassroomSessionBusinessImpl(ClassroomSessionDao dao) {
		super(dao);  
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession) {
		return dao.readByAcademicSession(academicSession);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public String format(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getName().getName()
				+(StringUtils.isBlank(classroomSession.getSuffix())?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE+classroomSession.getSuffix());
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession) {
		CommonNodeInformations commonNodeInformations = classroomSession.getLevelTimeDivision().getLevel().getName().getNodeInformations();
		if(commonNodeInformations==null)
			commonNodeInformations = classroomSession.getAcademicSession().getNodeInformations();
		if(commonNodeInformations==null)
			commonNodeInformations = classroomSession.getAcademicSession().getSchool().getNodeInformations();
		return commonNodeInformations;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond) {
		return millisecond==null?BigDecimal.ZERO
				:RootBusinessLayer.getInstance().getTimeDivisionTypeBusiness().convertToDivisionDuration(findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType(), millisecond);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration) {
		return duration==null?0l:RootBusinessLayer.getInstance().getTimeDivisionTypeBusiness().convertToMillisecond(findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType(), duration);
	}
	
	@Override
	protected void __load__(ClassroomSession classroomSession) {
		super.__load__(classroomSession);
		classroomSession.setDivisions(classroomSessionDivisionDao.readByClassroomSession(classroomSession));
	}

	

}
