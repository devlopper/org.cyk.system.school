package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;
import org.junit.Test;

public class IesaStudentBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Test
    public void crudPerson(){
    	TestCase testCase = instanciateTestCase();
    	Person person = inject(PersonBusiness.class).instanciateOneRandomly("P001");
    	testCase.create(person);
    	testCase.delete(person);
    	
    	person = inject(PersonBusiness.class).instanciateOneRandomly("P001");
    	testCase.create(person);
    	
    	testCase.clean();
    }
    
    @Test
    public void crudStudent(){
    	TestCase testCase = instanciateTestCase();
    	Student student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	//student.setImage(null);
    	testCase.create(student);
    	testCase.delete(student);
    	
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	//student.setImage(null);
    	testCase.create(student);
    	
    	testCase.clean();
    }
    
}
