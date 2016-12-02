package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.session.LevelBusiness;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.persistence.api.session.LevelDao;

public class LevelBusinessImpl extends AbstractTypedBusinessService<Level, LevelDao> implements LevelBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	public static Boolean PROPERTY_VALUE_TOKENS_CONCATENATE_WITH_GROUP_LEVELNAME_SPECIALITY = Boolean.TRUE;
	
	@Inject
	public LevelBusinessImpl(LevelDao dao) {
		super(dao);  
	}
	
	@Override
	protected Collection<? extends org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl.Listener<?>> getListeners() {
		return Listener.COLLECTION;
	}
	
	@Override
	protected Object[] getPropertyValueTokens(Level level, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			if(Boolean.TRUE.equals(PROPERTY_VALUE_TOKENS_CONCATENATE_WITH_GROUP_LEVELNAME_SPECIALITY))
				return new Object[]{level.getGroup(),level.getLevelName(),level.getSpeciality()};
			else{
				return new Object[]{level.getLevelName()};
			}
		return super.getPropertyValueTokens(level, name);
	}
	
	/**/
	
	public static interface Listener extends AbstractIdentifiableBusinessServiceImpl.Listener<Level> {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		public static class Adapter extends AbstractIdentifiableBusinessServiceImpl.Listener.Adapter.Default<Level> implements Listener,Serializable {
			private static final long serialVersionUID = 1L;
			
			/**/
			
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = 1L;
				
				
			}
		}
		
	}

}
