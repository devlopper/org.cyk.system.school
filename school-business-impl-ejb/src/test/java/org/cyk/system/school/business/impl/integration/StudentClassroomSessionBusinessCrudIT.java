package org.cyk.system.school.business.impl.integration;

import java.util.Arrays;
import java.util.Collection;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.iesa.AbstractIesaBusinessIT;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.CommonUtils;

public class StudentClassroomSessionBusinessCrudIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    /*@Override
	protected void populate() {
    	installApplication();
	}*/

	@Override
    protected void businesses() {
    		
    	Student student1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	student1.setCode("PK_STUD1");
    	student1.setName("komenan");
    	student1.getPerson().setLastnames("yao christian");
    	if(student1.getPerson().getContactCollection()!=null && student1.getPerson().getContactCollection().getElectronicMails()!=null)
    		student1.getPerson().getContactCollection().getElectronicMails().clear();
    	//inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	//inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(student1),AbstractIdentifiable.class));
    	
    	ClassroomSession classroomSession = inject(ClassroomSessionDao.class).readWhereSuffixIsNullByLevelName(SchoolConstant.Code.LevelName.K1).iterator().next();
    	
    	inject(GenericBusiness.class).create(new StudentClassroomSession(student1, classroomSession));
    	
    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionDao.class).readByStudentByClassroomSession(student1,classroomSession);
    	
    	inject(GenericBusiness.class).delete(studentClassroomSession);
    	
    	/**/
    	
    	student1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	student1.setCode("G1_STUD1");
    	student1.setName("komenan");
    	student1.getPerson().setLastnames("yao christian");
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(student1),AbstractIdentifiable.class));
    	classroomSession = inject(ClassroomSessionDao.class).readByLevelNameBySuffix(SchoolConstant.Code.LevelName.G1,"A").iterator().next();
    	studentClassroomSession = new StudentClassroomSession(student1, classroomSession);
    	inject(GenericBusiness.class).create(studentClassroomSession);
    	Collection<ClassroomSessionSubject> classroomSessionSubjects = inject(ClassroomSessionSubjectDao.class).readByClassroomSessionByStudent(classroomSession, student1);
    	
    	int count = 17;
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
    	
    	studentClassroomSession.getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(student1, studentClassroomSession.getClassroomSession())){
			studentClassroomSession.getDetailCollection().getCollection().add(studentClassroomSessionDivision);
			studentClassroomSessionDivision.getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
			for(ClassroomSessionSubject classroomSessionSubject : classroomSessionSubjects){
				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = inject(StudentClassroomSessionDivisionSubjectBusiness.class)
						.findByStudentByClassroomSessionDivisionBySubject(student1, studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject());
				if(studentClassroomSessionDivisionSubject==null)
					studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student1
							, inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionBySubject(studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject()));
				studentClassroomSessionDivision.getDetailCollection().getCollection().add(studentClassroomSessionDivisionSubject);
			}
		}
    	inject(GenericBusiness.class).update(studentClassroomSession);
    	count = 17;
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
    	
		
		studentClassroomSession.getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(student1, studentClassroomSession.getClassroomSession())){
			studentClassroomSession.getDetailCollection().getCollection().add(studentClassroomSessionDivision);
			studentClassroomSessionDivision.getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
			studentClassroomSessionDivision.getDetailCollection().getCollection().clear();
			for(ClassroomSessionSubject classroomSessionSubject : classroomSessionSubjects){
				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = inject(StudentClassroomSessionDivisionSubjectBusiness.class)
						.findByStudentByClassroomSessionDivisionBySubject(student1, studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject());
				if(studentClassroomSessionDivisionSubject==null)
					studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student1
							, inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionBySubject(studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject()));
				//studentClassroomSessionDivision.getDetailCollection().getCollection().add(studentClassroomSessionDivisionSubject);
			}
		}
		inject(GenericBusiness.class).update(studentClassroomSession);
		count = 0;
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
		assertEquals(count, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(student1
				, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l)).size());
    }
        
}
