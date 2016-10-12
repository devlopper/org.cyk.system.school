package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;

import org.cyk.system.root.business.api.language.LanguageCollectionBusiness;
import org.cyk.system.root.model.party.person.MedicalInformations;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;

public class StudentBusinessAdapter extends StudentBusinessImpl.Listener.Adapter.Default implements Serializable {
	
	private static final long serialVersionUID = 1L;

	public StudentBusinessAdapter() {
		addCascadeToClass(StudentClassroomSession.class).addCascadeToReportTemplateCodes(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE,
				SchoolConstant.REPORT_STUDENT_TUITION_CERTIFICATE);
	}
	
	@Override
	public void afterInstanciateOne(UserAccount userAccount, Student student) {
		super.afterInstanciateOne(userAccount, student);
		student.setStudentClassroomSession(new StudentClassroomSession(student, null));
		student.getPerson().getExtendedInformations().setLanguageCollection(inject(LanguageCollectionBusiness.class).instanciateOne(userAccount));
		student.getPerson().setMedicalInformations(new MedicalInformations(student.getPerson()));
	}
	
	@Override
	public void afterCreate(Student student) {
		super.afterCreate(student);
		/*Customer customer = new Customer();
		customer.setPerson(student.getPerson());
		customer.setCode(student.getCode());
		customer.setName(student.getName());
		inject(CustomerBusiness.class).create(customer);
		*/
	}
	
}
