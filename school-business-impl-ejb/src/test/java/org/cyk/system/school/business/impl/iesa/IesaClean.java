package org.cyk.system.school.business.impl.iesa;

import java.util.Collection;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

public class IesaClean extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
            
    @Override
    protected void businesses() {
    	Collection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class)
    			.readWhereStudentResultsEvaluationAverageIsNullByClassroomSessionDivisionOrderNumber(1l);
    	System.out.println("Trimester 1 : Not processed subjects : "+studentClassroomSessionDivisionSubjects.size());
    	for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : studentClassroomSessionDivisionSubjects){
    		//inject(StudentClassroomSessionDivisionSubjectBusiness.class).delete(studentClassroomSessionDivisionSubject);
    		ClassroomSessionDivision nextClassroomSessionDivision = inject(ClassroomSessionDivisionDao.class)
    				.readByClassroomSessionByOrderNumber(studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision()
    						.getClassroomSession(), studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()+2);
    		StudentClassroomSessionDivisionSubject sibling = inject(StudentClassroomSessionDivisionSubjectDao.class)
    				.readByStudentByClassroomSessionDivisionBySubject(studentClassroomSessionDivisionSubject.getStudent(), nextClassroomSessionDivision
    						, studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getSubject());
    		if(sibling!=null){
    			inject(StudentClassroomSessionDivisionSubjectBusiness.class).delete(sibling);
    			System.out.println("Deleted : "+sibling);
    		}
    	}
    	System.exit(0);
    }
    /*
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return super.getFakedDataProducer().setStructurationEnabled(Boolean.FALSE).setSynchronizationEnabled(Boolean.FALSE).setDoBusiness(Boolean.FALSE);
    }*/
}
