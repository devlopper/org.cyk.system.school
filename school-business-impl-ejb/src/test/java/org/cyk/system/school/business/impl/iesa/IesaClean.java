package org.cyk.system.school.business.impl.iesa;

import java.util.Collection;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

public class IesaClean extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
            
    @Override
    protected void businesses() {
    	/*Collection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class)
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
    	}*/
    	try{
    		updateClassroomSessionDivisionSubjectsFrom2To3();
    	}catch(Exception exception){
    		exception.printStackTrace();
    	}
    	System.exit(0);
    }
    
    private void updateClassroomSessionDivisionSubjectsFrom2To3(){
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects2 = inject(ClassroomSessionDivisionSubjectDao.class)
    			.readByClassroomSessionDivisionOrderNumber(2l);
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects3 = inject(ClassroomSessionDivisionSubjectDao.class)
    			.readByClassroomSessionDivisionOrderNumber(3l);
    	
    	for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject3 : classroomSessionDivisionSubjects3){
    		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject2 : classroomSessionDivisionSubjects2){
        		if(classroomSessionDivisionSubject2.getClassroomSessionDivision().getClassroomSession()
        				.equals(classroomSessionDivisionSubject3.getClassroomSessionDivision().getClassroomSession()) && 
        				classroomSessionDivisionSubject2.getSubject().equals(classroomSessionDivisionSubject3.getSubject()) ){
        			classroomSessionDivisionSubject3.setTeacher(classroomSessionDivisionSubject2.getTeacher());
        			break;
        		}
        	}
    	}
    	
    	inject(ClassroomSessionDivisionSubjectBusiness.class).update(classroomSessionDivisionSubjects3);
    }
    
    @Override
    protected void installApplication(Boolean fake) {}
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return super.getFakedDataProducer().setStructurationEnabled(Boolean.FALSE).setSynchronizationEnabled(Boolean.FALSE).setDoBusiness(Boolean.FALSE);
    }
}
