package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.api.value.MeasureBusiness;
import org.cyk.system.root.business.impl.time.AbstractIdentifiablePeriodBusinessImpl;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.session.LevelNameBusiness;
import org.cyk.system.school.business.api.session.SchoolBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;
import org.cyk.utility.common.helper.FieldHelper;

public class AcademicSessionBusinessImpl extends AbstractIdentifiablePeriodBusinessImpl<AcademicSession, AcademicSessionDao> implements AcademicSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public AcademicSessionBusinessImpl(AcademicSessionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Object[] getPropertyValueTokens(AcademicSession academicSession,String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{timeBusiness.formatDate(academicSession.getExistencePeriod().getFromDate())
					,timeBusiness.formatDate(academicSession.getExistencePeriod().getToDate())};
		return super.getPropertyValueTokens(academicSession, name);
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
		TimeDivisionType timeDivisionType = findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType();
		return millisecond == null ? BigDecimal.ZERO : inject(MeasureBusiness.class).computeQuotient(timeDivisionType.getMeasure(), new BigDecimal(millisecond));
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long convertAttendanceTimeToMillisecond(BigDecimal duration) {
		TimeDivisionType timeDivisionType = findCurrent(null).getNodeInformations().getAttendanceTimeDivisionType();
		return duration == null ? 0 : inject(MeasureBusiness.class).computeMultiple(timeDivisionType.getMeasure(), duration).longValue();
	}
	
	@Override
	protected void beforeCreate(AcademicSession academicSession) {
		super.beforeCreate(academicSession);
		if(academicSession.getNodeInformations().getAttendanceTimeDivisionType()==null)
			academicSession.getNodeInformations().set(academicSession.getSchool().getNodeInformations());
	}
	
	@Override
	protected void afterUpdate(AcademicSession academicSession) {
		super.afterUpdate(academicSession);
		FieldHelper fieldHelper = new FieldHelper();
		if(academicSession.getLevelGroups().isSynchonizationEnabled()){
			for(LevelGroup levelGroup : academicSession.getLevelGroups().getCollection()){
				copy(academicSession, levelGroup.getNodeInformations(),academicSession.getLevelGroups().getFieldNames(), fieldHelper);
			}	
			inject(LevelGroupBusiness.class).update(academicSession.getLevelGroups().getCollection());
		}
		if(academicSession.getLevelNames().isSynchonizationEnabled()){
			for(LevelName levelName : academicSession.getLevelNames().getCollection()){
				copy(academicSession, levelName.getNodeInformations(),academicSession.getLevelNames().getFieldNames(), fieldHelper);
			}
			inject(LevelNameBusiness.class).update(academicSession.getLevelNames().getCollection());
		}
		
	}
	
	private void copy(AcademicSession academicSession,CommonNodeInformations destination,Collection<String> fieldNames,FieldHelper fieldHelper){
		fieldHelper.copy(academicSession.getNodeInformations(), destination, fieldNames /*CommonNodeInformations.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX*/);	
	}
		
	@Override
	public AcademicSession instanciateOne() {
		AcademicSession academicSession = super.instanciateOne();
		academicSession.setSchool(inject(SchoolBusiness.class).findDefault());
		return academicSession;
	}
	
	@Override
	public AcademicSession instanciateOne(String[] values) {
		AcademicSession academicSession = instanciateOne();
		Integer index = 0;
		academicSession.setCode(values[index++]);
		academicSession.setName(values[index++]);
		academicSession.getGlobalIdentifierCreateIfNull().getExistencePeriod().setFromDate(inject(TimeBusiness.class).parse(values[index++]));
    	academicSession.getExistencePeriod().setToDate(inject(TimeBusiness.class).parse(values[index++]));
		return academicSession;
	}
}
