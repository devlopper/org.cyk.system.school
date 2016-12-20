package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.session.LevelTimeDivisionBusiness;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.persistence.api.session.LevelDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;

public class LevelTimeDivisionBusinessImpl extends AbstractTypedBusinessService<LevelTimeDivision, LevelTimeDivisionDao> implements LevelTimeDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	public static Boolean PROPERTY_VALUE_TOKENS_CONCATENATE_WITH_TIMEDIVISIONTYPE = Boolean.TRUE;
	
	@Inject
	public LevelTimeDivisionBusinessImpl(LevelTimeDivisionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Collection<? extends org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl.Listener<?>> getListeners() {
		return Listener.COLLECTION;
	}
	
	@Override
	protected void beforeCreate(LevelTimeDivision levelTimeDivision) {
		super.beforeCreate(levelTimeDivision);
		exceptionUtils().exists(dao.readByLevelByTimeDivision(levelTimeDivision.getLevel(), levelTimeDivision.getTimeDivisionType()));
	}
	
	@Override
	protected Object[] getPropertyValueTokens(LevelTimeDivision levelTimeDivision, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			if(Boolean.TRUE.equals(PROPERTY_VALUE_TOKENS_CONCATENATE_WITH_TIMEDIVISIONTYPE))
				return new Object[]{levelTimeDivision.getLevel(),levelTimeDivision.getTimeDivisionType()};
			else{
				return new Object[]{levelTimeDivision.getLevel()};
			}
		return super.getPropertyValueTokens(levelTimeDivision, name);
	}
	
	public LevelTimeDivision instanciateOne(String code, String levelCode, String timeDivisionTypeCode, Long orderNumber) {
		LevelTimeDivision levelTimeDivision = new LevelTimeDivision(timeDivisionTypeCode, inject(LevelDao.class).read(levelCode)
				, inject(TimeDivisionTypeDao.class).read(timeDivisionTypeCode), orderNumber);
		return levelTimeDivision;
	}
	
	@Override
	public LevelTimeDivision findByLevelByTimeDivision(Level level, TimeDivisionType timeDivisionType) {
		return dao.readByLevelByTimeDivision(level,timeDivisionType);
	}
	
	@Override
	public LevelTimeDivision instanciateOne(String[] values) {
		LevelTimeDivision levelTimeDivision = instanciateOne();
		levelTimeDivision.setCode(values[0]);
		levelTimeDivision.setLevel(inject(LevelDao.class).read(values[1]));
		levelTimeDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(values[2]));
		return levelTimeDivision;
	}
	
	/**/
	
	public static interface Listener extends AbstractIdentifiableBusinessServiceImpl.Listener<LevelTimeDivision> {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		public static class Adapter extends AbstractIdentifiableBusinessServiceImpl.Listener.Adapter<LevelTimeDivision> implements Listener,Serializable {
			private static final long serialVersionUID = 1L;
			
			/**/
			
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = 1L;
				
				
			}
		}
		
	}
	
}
