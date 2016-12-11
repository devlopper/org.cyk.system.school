package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.actor.Teacher.SearchCriteria;
import org.cyk.system.school.persistence.api.actor.TeacherDao;

import lombok.Getter;
import lombok.Setter;

public class TeacherBusinessImpl extends AbstractActorBusinessImpl<Teacher, TeacherDao,SearchCriteria> implements TeacherBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public TeacherBusinessImpl(TeacherDao dao) {
		super(dao);  
	}
	
	@Override
	protected Collection<? extends org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl.Listener<?>> getListeners() {
		return Listener.COLLECTION;
	}

	/**/

	public static interface Listener extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener<Teacher>{
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/

		public static class Adapter extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener.Adapter.Default<Teacher> implements Listener, Serializable {
			private static final long serialVersionUID = -1625238619828187690L;
			
			/**/
			@Getter @Setter
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = -1625238619828187690L;
				
				/**/
				
				public static class EnterpriseResourcePlanning extends TeacherBusinessImpl.Listener.Adapter.Default implements Serializable {
					
					private static final long serialVersionUID = 1L;

					public EnterpriseResourcePlanning() {
						
					}
					
				}

				
			}
			
		}
		
	}	

    
}
