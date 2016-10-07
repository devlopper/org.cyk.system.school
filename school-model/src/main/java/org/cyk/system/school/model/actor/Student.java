package org.cyk.system.school.model.actor;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @Entity @ModelBean(genderType=GenderType.MALE,crudStrategy=CrudStrategy.BUSINESS)
public class Student extends AbstractActor implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private LevelTimeDivision admissionLevelTimeDivision;
	
	@Transient private StudentClassroomSession studentClassroomSession;
	
	/**/
	
	public static class SearchCriteria extends AbstractSearchCriteria<Student> {

		private static final long serialVersionUID = -7909506438091294611L;

		public SearchCriteria() {
			this(null);
		}

		public SearchCriteria(String name) {
			super(name);
		}
		
		
	}

	
}
