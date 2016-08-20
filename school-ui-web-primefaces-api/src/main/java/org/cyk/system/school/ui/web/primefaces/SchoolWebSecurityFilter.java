package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import org.cyk.ui.web.api.servlet.SecurityFilter;

public class SchoolWebSecurityFilter extends SecurityFilter.Listener.Adapter.Default implements Serializable{
	
	private static final long serialVersionUID = -8581044465789806149L;
	
	/*@Override
	public Boolean isUrlAccessibleByUserAccount(URL url, UserAccount userAccount) {
		if(Boolean.TRUE.equals(super.isUrlAccessibleByUserAccount(url, userAccount))){
			Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userAccount.getUser());
			if(teacher==null)
				return Boolean.FALSE;
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		}
		return Boolean.FALSE;
	}*/
	
	/**/
}