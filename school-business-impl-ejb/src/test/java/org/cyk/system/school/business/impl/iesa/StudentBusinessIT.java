package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;

public class StudentBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	Student student;
    	
    	assertEquals(0l, inject(StudentBusiness.class).countAll());
    	assertEquals(0l, inject(StudentClassroomSessionBusiness.class).countAll());
    	
    	student = inject(StudentBusiness.class).instanciateOne();
    	student.setStudentClassroomSession(new StudentClassroomSession(student, inject(ClassroomSessionDao.class).readOneRandomly()));
    	create(student);
    	assertEquals(1l, inject(StudentBusiness.class).countAll());
    	assertEquals(1l, inject(StudentClassroomSessionBusiness.class).countAll());
    	
    	student = inject(StudentBusiness.class).instanciateOne();
    	student.setStudentClassroomSession(new StudentClassroomSession(student, inject(ClassroomSessionDao.class).readOneRandomly()));
    	create(student);
    	assertEquals(2l, inject(StudentBusiness.class).countAll());
    	assertEquals(2l, inject(StudentClassroomSessionBusiness.class).countAll());
    }
    
    

}
