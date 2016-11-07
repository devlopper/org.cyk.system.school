package org.cyk.system.school.ui.web.primefaces.page;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.model.structure.Company;
import org.cyk.system.root.model.geography.ContactCollection;
import org.cyk.system.root.model.party.person.JobFunction;
import org.cyk.system.root.model.party.person.PersonTitle;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.model.party.AbstractActorEditFormModel;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.party.AbstractActorEditPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.InputOneRadio;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.cdi.AbstractBean;

@Named @ViewScoped @Getter @Setter
public class StudentEditPage extends AbstractActorEditPage.AbstractDefault.Default<Student> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	FormOneData<ContactsForm> contactsForm;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contactsForm = (FormOneData<ContactsForm>) createFormOneData(new ContactsForm(), crud);
		contactsForm.setTabTitle("Contacts");
	}
	
	@Override
	protected void create() {
		//super.create();
		debug(contactsForm.getData());
	}
	
	public static class Form extends AbstractActorEditFormModel.AbstractDefault.Default<Student> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private LevelTimeDivision admissionLevelTimeDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		
		@Override
		public void read() {
			super.read();
			if(identifiable.getStudentClassroomSession()!=null)
				classroomSession = identifiable.getStudentClassroomSession().getClassroomSession();
		}
		
		@Override
		public void write() {
			super.write();
			if(identifiable.getStudentClassroomSession()!=null)
				identifiable.getStudentClassroomSession().setClassroomSession(classroomSession);
		}
		
		public static final String FIELD_ADMISSION_LEVEL_TIME_DIVISION = "admissionLevelTimeDivision";
		public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	}
	
	public static class ContactsForm extends AbstractBean implements Serializable {
		private static final long serialVersionUID = -751917271358280700L;
		
		@Input @InputText String father;
		@Input @InputText String mother;
		
		/**/
		
		public static class Parent extends AbstractBean implements Serializable {
			private static final long serialVersionUID = 1L;
			
			@Input @InputChoice @InputOneChoice @InputOneRadio private PersonTitle title;
			@Input @InputText private String names;
			@Input @InputText private String mobilePhoneNumber;
			@Input @InputText private String email;
			
			@Input @InputText private String homeAddress;
			@Input @InputText private String homePostCode;
			@Input @InputText private String homePhoneNumber;
			
			@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Company company;
			@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private JobFunction jobFunction;
			@Input @InputText private String workAddress;
			@Input @InputText private String workPhoneNumber;
			@Input @InputText private String workPostCode;
		}
		
	}

}
