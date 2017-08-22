package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.session.LevelSpecialityBusiness;
import org.cyk.system.school.model.session.LevelSpeciality;
import org.cyk.system.school.persistence.api.session.LevelSpecialityDao;

public class LevelSpecialityBusinessImpl extends AbstractEnumerationBusinessImpl<LevelSpeciality, LevelSpecialityDao> implements LevelSpecialityBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelSpecialityBusinessImpl(LevelSpecialityDao dao) {
		super(dao); 
	}   
	
	public static class BuilderOneDimensionArray extends AbstractEnumerationBusinessImpl.BuilderOneDimensionArray<LevelSpeciality> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(LevelSpeciality.class);
		}
		
	}
	
}
