package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;

@Singleton
public class SchoolBusinessTestHelper extends AbstractTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	
	/**/
	
	public Student registerStudent(String code,String[] names){
		Student student = RootRandomDataProvider.getInstance().actor(Student.class);
		student.getRegistration().setCode(code);
		if(names!=null){
			if(names.length>0)
				student.getPerson().setName(names[0]);
			if(names.length>1)
				student.getPerson().setLastName(names[1]);
			if(names.length>2)
				student.getPerson().setSurname(names[2]);
		}
		return studentBusiness.create(student);
	}
	
	public void takeSubjects(String[] studentRegistrationCodes,Subject[] subjects){
		for(String studentRegistrationCode : studentRegistrationCodes){
			Student student = studentBusiness.findByRegistrationCode(studentRegistrationCode);
			for(Subject subject : subjects){
				StudentSubject studentSubject = new StudentSubject(student, subject);
				studentSubjectBusiness.create(studentSubject);
			}
		}
	}
	
	/**/
	
	public static SchoolBusinessTestHelper getInstance() {
		return INSTANCE;
	}
	
}
