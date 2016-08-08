package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.time.TimeDivisionTypeBusiness;
import org.cyk.system.root.business.impl.time.AbstractIdentifiablePeriodBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;

@Stateless
public class AcademicSessionBusinessImpl extends AbstractIdentifiablePeriodBusinessImpl<AcademicSession, AcademicSessionDao> implements AcademicSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private TimeDivisionTypeBusiness timeDivisionTypeBusiness;
	
	@Inject
	public AcademicSessionBusinessImpl(AcademicSessionDao dao) {
		super(dao);  
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public AcademicSession findCurrent(School school) {//TODO do better
		ArrayList<AcademicSession> list = new ArrayList<>(findAll());
		if(list.isEmpty())
			return null;
		return list.get(list.size()-1);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public BigDecimal convertAttendanceTimeToDivisionDuration(Long millisecond) {
		return millisecond==null?BigDecimal.ZERO
				:timeDivisionTypeBusiness.convertToDivisionDuration(findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType(), millisecond);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long convertAttendanceTimeToMillisecond(BigDecimal duration) {
		return duration==null?0:timeDivisionTypeBusiness.convertToMillisecond(findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType(), duration);
	}
	
	@Override
	public AcademicSession update(AcademicSession academicSession,Boolean cascade) {
		academicSession = update(academicSession);
		for(AbstractIdentifiable identifiable : genericDao.use(LevelName.class).select().all()){
			LevelName levelName = (LevelName) identifiable;
			//TODO a attribute value copy method should be developed
			levelName.getNodeInformations().setCurrentClassroomSessionDivisionIndex(academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
			//TODO all attribute should be copied
			genericDao.update(levelName);
		}
		return academicSession;
	}

}
