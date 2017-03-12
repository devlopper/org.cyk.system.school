package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.CommonUtils;

public class FakedIesaApplicationSetupBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	
    	ClassroomSession classroomSessionPK = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1, null);
    
    	ClassroomSession classroomSessionK1 = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1, null);
    	
    	ClassroomSession classroomSessionK2 = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1, null);
    	
    	ClassroomSession classroomSessionK3 = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1, null);
    	
    	ClassroomSession classroomSessionG1A = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, SchoolConstant.Code.ClassroomSessionSuffix.A);
    	
    	ClassroomSession classroomSessionG8A = inject(ClassroomSessionBusiness.class)
    			.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1, null);
    	
    	Student studentG1A = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentG1A.setCode("STUDG1A");
    	studentG1A.setName("komenan");
    	studentG1A.getPerson().setLastnames("yao christian");
    	if(studentG1A.getPerson().getContactCollection()!=null && studentG1A.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentG1A.getPerson().getContactCollection().getElectronicMails().clear();
    	inject(ElectronicMailBusiness.class).setAddress(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	inject(ElectronicMailBusiness.class).setAddress(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
    	Student studentG8A = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentG8A.setCode("STUDG8A");
    	studentG8A.setName("zadi");
    	studentG8A.getPerson().setLastnames("gérard");
    	if(studentG8A.getPerson().getContactCollection()!=null && studentG8A.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentG8A.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentPK1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentPK1.setCode("STUDPK");
    	studentPK1.setName("Bartheon");
    	studentPK1.getPerson().setLastnames("Robert");
    	if(studentPK1.getPerson().getContactCollection()!=null && studentPK1.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentPK1.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK1.setCode("STUDK1");
    	studentK1.setName("Cecile");
    	studentK1.getPerson().setLastnames("Jack");
    	if(studentK1.getPerson().getContactCollection()!=null && studentK1.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK1.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK2 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK2.setCode("STUDK2");
    	studentK2.setName("Mamadou");
    	studentK2.getPerson().setLastnames("kone");
    	if(studentK2.getPerson().getContactCollection()!=null && studentK2.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK2.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK3 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK3.setCode("STUDK3");
    	studentK3.setName("Stack");
    	studentK3.getPerson().setLastnames("ariel");
    	if(studentK3.getPerson().getContactCollection()!=null && studentK3.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK3.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(studentPK1,studentK1,studentK2,studentK3,studentG1A,studentG8A)
    			,AbstractIdentifiable.class));
    	
    	Person parent = inject(PersonBusiness.class).findOneByPersonByRelationshipType(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER);
    	parent.setName("komenan");
    	parent.setLastnames("n'dri jean");
    	//parent.getContactCollection().getcoll
    	update(parent);
    	
    	parent = inject(PersonBusiness.class).findOneByPersonByRelationshipType(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER);
    	parent.setName("komenan");
    	parent.setLastnames("sandrine meliane");
    	update(parent);
    	
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentPK1.getCode(),classroomSessionPK.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK1.getCode(),classroomSessionK1.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK2.getCode(),classroomSessionK2.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK3.getCode(),classroomSessionK3.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG1A.getCode(),classroomSessionG1A.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG8A.getCode(),classroomSessionG8A.getCode()}));
    	
    	
    	System.exit(0);
    }
    
}
