package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.net.URL;

import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.web.api.servlet.SecurityFilter;

public class SchoolWebSecurityFilter extends SecurityFilter.Listener.Adapter.Default implements Serializable{
	
	private static final long serialVersionUID = -8581044465789806149L;
	
	@Override
	public Boolean isUrlAccessibleByUserAccount(URL url, UserAccount userAccount) {
		if(Boolean.TRUE.equals(super.isUrlAccessibleByUserAccount(url, userAccount))){
			Teacher teacher = SchoolBusinessLayer.getInstance().getTeacherBusiness().findByPerson((Person) userAccount.getUser());
			if(teacher==null)
				return Boolean.FALSE;
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		}
		return Boolean.FALSE;
	}
	
	/**/
}