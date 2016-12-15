package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.utility.common.CommonUtils;

public class StudentClassroomSessionBusinessCrudIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	installApplication();
    	
    	Student student1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	student1.setCode("STUD1");
    	student1.setName("komenan");
    	student1.getPerson().setLastnames("yao christian");
    	if(student1.getPerson().getContactCollection()!=null && student1.getPerson().getContactCollection().getElectronicMails()!=null)
    		student1.getPerson().getContactCollection().getElectronicMails().clear();
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(student1),AbstractIdentifiable.class));
    	
    	ClassroomSession classroomSession = inject(ClassroomSessionDao.class).readWhereSuffixIsNullByLevelName(SchoolConstant.Code.LevelName.K1).iterator().next();
    	
    	inject(GenericBusiness.class).create(new StudentClassroomSession(student1, classroomSession));
    	
    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionDao.class).readByStudentByClassroomSession(student1,classroomSession);
    	
    	inject(GenericBusiness.class).delete(studentClassroomSession);
    	
    	
    }
        
}
