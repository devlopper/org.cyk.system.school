package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;
import org.junit.Test;

public class IesaStudentBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Test
    public void crudStudent(){
    	TestCase testCase = instanciateTestCase();
    	Student student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	testCase.create(student);
    	testCase.delete(student);
    	testCase.clean();
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	testCase.delete(student);
    }
    
}
