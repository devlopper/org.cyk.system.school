package org.cyk.system.school.business.impl.iesa;

import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

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
    		//updateClassroomSessionDivisionSubjectsFrom2To3();
    		createStudentClassroomSessionDivisionFrom2To3();
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
    
    private void createStudentClassroomSessionDivisionFrom2To3(){
    	System.out.println("IesaClean.createStudentClassroomSessionDivisionFrom2To3() : Starts");
    	System.out.println("Count : 2 : "+inject(StudentClassroomSessionDivisionDao.class).countByClassroomSessionDivisionIndex(2l)+" : 3 : "
    			+inject(StudentClassroomSessionDivisionDao.class).countByClassroomSessionDivisionIndex(3l));
    	/*
    	inject(StudentClassroomSessionDivisionBusiness.class).create(new StudentClassroomSessionDivision(inject(StudentDao.class).read("IESA/2016ICM0876-KG")
    			, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(inject(ClassroomSessionDao.class)
    					.readByLevelName(SchoolConstant.Code.LevelName.PK).iterator().next(), 3l)));
    	*/
    	/*
    	Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions2 = inject(StudentClassroomSessionDivisionDao.class)
    			.readByClassroomSessionDivisionIndex(2l);
    	Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions3 = inject(StudentClassroomSessionDivisionDao.class)
    			.readByClassroomSessionDivisionIndex(3l);
    	Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
    	System.out.println("2 : "+studentClassroomSessionDivisions2.size());
    	System.out.println("3 : "+studentClassroomSessionDivisions3.size());
    	for(StudentClassroomSessionDivision studentClassroomSessionDivision2 : studentClassroomSessionDivisions2){
    		Boolean found = Boolean.FALSE;
    		for(StudentClassroomSessionDivision studentClassroomSessionDivision3 : studentClassroomSessionDivisions3){
        		if(studentClassroomSessionDivision2.getClassroomSessionDivision().getClassroomSession()
        				.equals(studentClassroomSessionDivision3.getClassroomSessionDivision().getClassroomSession()) && 
        				studentClassroomSessionDivision2.getStudent().equals(studentClassroomSessionDivision3.getStudent()) ){
        			found = Boolean.TRUE;
        			break;
        		}
        	}
    		if(Boolean.FALSE.equals(found)){
    			studentClassroomSessionDivisions.add(new StudentClassroomSessionDivision(studentClassroomSessionDivision2.getStudent(), studentClassroomSessionDivision2.getClassroomSessionDivision()));
    		}
    	}
    	
    	System.out.println("Number of new studentClassroomSessionDivisions : "+studentClassroomSessionDivisions.size());
    	*/
    	//inject(StudentClassroomSessionDivisionBusiness.class).create(studentClassroomSessionDivisions);
    	
    	System.out.println("Done!");
    }
    
    @Override
    protected void installApplication(Boolean fake) {}
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return super.getFakedDataProducer().setStructurationEnabled(Boolean.FALSE).setSynchronizationEnabled(Boolean.FALSE).setDoBusiness(Boolean.FALSE);
    }
}
