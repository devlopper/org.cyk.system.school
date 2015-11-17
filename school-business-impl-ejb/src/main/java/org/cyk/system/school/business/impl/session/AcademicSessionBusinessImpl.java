package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;

import javax.inject.Inject;

import org.cyk.system.root.business.api.time.TimeDivisionTypeBusiness;
import org.cyk.system.root.business.impl.event.AbstractIdentifiablePeriodBusinessImpl;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;

public class AcademicSessionBusinessImpl extends AbstractIdentifiablePeriodBusinessImpl<AcademicSession, AcademicSessionDao> implements AcademicSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private TimeDivisionTypeBusiness timeDivisionTypeBusiness;
	
	@Inject
	public AcademicSessionBusinessImpl(AcademicSessionDao dao) {
		super(dao);  
	}
	
	@Override
	public AcademicSession findCurrent(School school) {//TODO do better
		ArrayList<AcademicSession> list = new ArrayList<>(findAll());
		if(list.isEmpty())
			return null;
		return list.get(list.size()-1);
	}

	@Override
	public BigDecimal convertAttendanceTimeToDivisionDuration(Long millisecond) {
		return timeDivisionTypeBusiness.convertToDivisionDuration(findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType(), millisecond);
	}

	@Override
	public Long convertAttendanceTimeToMillisecond(BigDecimal duration) {
		return timeDivisionTypeBusiness.convertToMillisecond(findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType(), duration);
	}

}
