package org.cyk.system.school.model.actor;

import java.io.Serializable;

import javax.persistence.Entity;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @ModelBean(genderType=GenderType.MALE,crudStrategy=CrudStrategy.BUSINESS)
public class Teacher extends AbstractActor implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;


	/**/
	
	public static class SearchCriteria extends AbstractSearchCriteria<Teacher> {

		private static final long serialVersionUID = -7909506438091294611L;

		public SearchCriteria() {
			this(null);
		}

		public SearchCriteria(String name) {
			super(name);
		}
		
		
	}
}
