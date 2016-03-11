package org.cyk.system.school.business.impl.integration;

import javax.inject.Inject;

import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;

public class InstanciationBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private StudentBusiness studentBusiness;
    
    @Override
    protected void finds() {
        
    }

    @Override
    protected void businesses() {
    	installApplication();
    	Student student;
    	
    	student = new Student();
    	student.setPerson(new Person());
    	student.getRegistration().setCode("STUD001");
    	student.getPerson().setName("MYNAME");
    	
    	studentBusiness.completeInstanciationOfOne(student);
    	
    	studentBusiness.create(student);
    	
    }

}
