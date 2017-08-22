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
import org.cyk.system.school.persistence.api.session.LevelGroupDao;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.cyk.system.school.persistence.api.session.LevelSpecialityDao;

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
			return new Object[]{level.getLevelName(),level.getSpeciality()};
		return super.getPropertyValueTokens(level, name);
	}
	
	@Override
	public Level instanciateOne(String levelGroupCode, String levelNameCode, String levelSpecialityCode) {
		Level level = new Level(inject(LevelGroupDao.class).read(levelGroupCode), inject(LevelNameDao.class).read(levelNameCode)
				, inject(LevelSpecialityDao.class).read(levelSpecialityCode));
		return level;
	}
	
	@Override
	protected Level __instanciateOne__(String[] values,org.cyk.system.root.business.api.TypedBusiness.InstanciateOneListener<Level> listener) {
		Level level = super.__instanciateOne__(values, listener);
		set(listener.getSetListener(), Level.FIELD_LEVEL_NAME);
		set(listener.getSetListener(), Level.FIELD_LEVEL_SPECIALITY);
		set(listener.getSetListener(), Level.FIELD_GROUP);
		return level;
	}

	@Override
	public Level instanciateOne(String[] values) {
		Level level = instanciateOne();
		Integer index = 0;
		level.setLevelName(inject(LevelNameDao.class).read(values[index++]));
		level.setSpeciality(inject(LevelSpecialityDao.class).read(values[index++]));
		level.setGroup(inject(LevelGroupDao.class).read(values[index++]));
		return level;
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

	/**/
	
	public static class BuilderOneDimensionArray extends org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<Level> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(Level.class);
			addParameterArrayElementString(Level.FIELD_LEVEL_NAME,Level.FIELD_LEVEL_SPECIALITY,Level.FIELD_GROUP);
		}
		
	}
}
