package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.ui.web.primefaces.test.automation.Form;
import org.cyk.ui.web.primefaces.test.automation.IdentifiableWebITRunner;

public class StudentWebITRunner extends IdentifiableWebITRunner<Student> implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public String[] getListMenuItemPath() {
		return new String[]{"commandable_student_management_","commandable_list_student_"};
	}
	@Override
	public String getCode(Crud crud) {
		switch(crud){
		case CREATE:return "stud001";
		case READ:return "stud001";
		case UPDATE:return "stud001";
		case DELETE:return "stud001";
		}
		return null;
	}
	@Override
	public void fillForm(Form form,Crud crud) {
		switch(crud){
		case CREATE:
			form.addInputText(StudentEditPage.Form.FIELD_NAME, "Zadi")
		        ;
			break;
		case READ:
			break;
		case UPDATE:
			form.addInputText(StudentEditPage.Form.FIELD_NAME, "Gohou")
				;
			break;
		case DELETE:
			break;
		}
	}

}
