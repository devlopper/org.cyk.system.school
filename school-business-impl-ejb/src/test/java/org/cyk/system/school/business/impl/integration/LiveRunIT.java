package org.cyk.system.school.business.impl.integration;

import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.school.business.impl.iesa.AbstractIesaBusinessIT;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

public class LiveRunIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	try {
			System.out.println(inject(StudentClassroomSessionDivisionSubjectDao.class).countAll());
			Collection<StudentClassroomSessionDivisionSubject> collection = inject(StudentClassroomSessionDivisionSubjectDao.class).readDuplicates();
			System.out.println(collection.size());
			//for(StudentClassroomSessionDivisionSubject i : collection)
			//	System.out.println(i);
			Collection<StudentClassroomSessionDivisionSubject> collection2 = new ArrayList<>();
			for(StudentClassroomSessionDivisionSubject i : collection)
				for(StudentClassroomSessionDivisionSubject k : inject(StudentClassroomSessionDivisionSubjectDao.class).readDuplicatesByStudentByClassroomSessionDivisionBySubject(
						i.getStudent(), i.getClassroomSessionDivisionSubject().getClassroomSessionDivision(), i.getClassroomSessionDivisionSubject().getSubject()))
				collection2.add(k);
			System.out.println(collection2.size());
		} catch (Exception e) {
			e.printStackTrace();
		} finally{
			System.exit(0);
		}
    	
    	
    }
        
}
