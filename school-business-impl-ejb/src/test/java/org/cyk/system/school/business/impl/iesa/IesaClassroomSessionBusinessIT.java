package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.junit.Test;

public class IesaClassroomSessionBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
            
    @Test
    public void crudOnlyClassroom(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
    	testCase.create(classroomSession);
    	assertEquals(0l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	testCase.clean();
    }
    
    @Test
    public void crudClassroomAndDivisions(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.getNodeInformations().setClassroomSessionTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l));
    	classroomSession.getDivisions().addOne(inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l));
    	classroomSession.getDivisions().addOne(inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l));
    	testCase.create(classroomSession);
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	testCase.clean();
    }
    
    @Test
    public void crudClassroomAndDivisionsAndSubject(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.getNodeInformations().setClassroomSessionTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
    	classroomSession.getDivisions().addOne(classroomSessionDivision1 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision2 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision3 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l));
    	classroomSessionDivision1.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision1.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision1,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivision2.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision2.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision2,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS)));
    	classroomSessionDivision3.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision3.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivision3.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY)));
    	
    	testCase.create(classroomSession);
    	
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(4, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(3, inject(ClassroomSessionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	
    	Student student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	//student.setImage(null);
    	testCase.create(student);
    	StudentClassroomSession studentClassroomSession = new StudentClassroomSession(student, classroomSession);
    	testCase.create(studentClassroomSession);
    	
    	testCase.clean();
    }
    
    @Test
    public void deleteG8Division(){    	
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix(SchoolConstant.Code.Level.G8,null).iterator().next();
    	inject(GenericBusiness.class).delete(inject(ClassroomSessionDivisionDao.class).readByClassroomSession(classroomSession).iterator().next());
    }
    
    @Test
    public void deleteG9(){
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix(SchoolConstant.Code.Level.G9,null).iterator().next();
    	inject(GenericBusiness.class).delete(classroomSession);
    }
    
    @Test
    public void crudClassroomAndDivisionsAndSubjectAndEvaluationType(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.getNodeInformations().setClassroomSessionTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
    	classroomSession.getDivisions().addOne(classroomSessionDivision1 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision2 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision3 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l));
    	classroomSessionDivision1.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
    	classroomSessionDivision1.getSubjects().addOne(classroomSessionDivisionSubject = inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision1,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivisionSubject.getEvaluationTypes().addOne(inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).instancaiteOne(classroomSessionDivisionSubject
    			, inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST1)));
    	classroomSessionDivision2.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision2.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision2,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS)));
    	classroomSessionDivision3.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision3.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT)));
    	classroomSessionDivision3.getSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY)));
    	
    	testCase.create(classroomSession);
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(4, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	testCase.clean();
    }
    
    @Test
    public void requiredSubjects(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
    	
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	
    	classroomSession.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionSubject classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	
    	classroomSession.getStudents().setSynchonizationEnabled(Boolean.TRUE);
    	Student student = inject(StudentBusiness.class).instanciateOneRandomly("S00A1");
    	testCase.create(student);
    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S00A2");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S00A3");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	
    	testCase.create(classroomSession);
    	assertEquals(4, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionByRequired(classroomSessionDivision,Boolean.TRUE).size());
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(12, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(3, inject(StudentClassroomSessionDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(9, inject(StudentClassroomSessionDivisionDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(36, inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	
    	/**/
    	
    	classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1));
    	
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	
    	classroomSession.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	
    	classroomSession.getStudents().setSynchonizationEnabled(Boolean.TRUE);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S00B1");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S00B2");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S00B3");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCascadeOperationToChildren(Boolean.TRUE);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	
    	testCase.create(classroomSession);
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(12, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(0, inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	
    	testCase.clean();
    }
    
    @Test
    public void createClassroom(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.FALSE);
    	testCase.create(classroomSession);
    	assertEquals(0l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	
    	classroomSession = inject(ClassroomSessionBusiness.class).findByLevelName(SchoolConstant.Code.LevelName.G1).iterator().next();
    	ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession, 1l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.getSubjects().setSynchonizationEnabled(Boolean.FALSE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession, 2l);
    	classroomSessionDivision.setTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSessionDivision.getSubjects().setSynchonizationEnabled(Boolean.FALSE);
    	classroomSession.getDivisions().addOne(classroomSessionDivision);
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	testCase.update(classroomSession);
    	assertEquals(2l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	
    	classroomSession = inject(ClassroomSessionBusiness.class).findByLevelName(SchoolConstant.Code.LevelName.G1).iterator().next();
    	ClassroomSessionSubject classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession
    			, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession
    			, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class).instanciateOne(classroomSession
    			, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT));
    	classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
    	classroomSession.getSubjects().addOne(classroomSessionSubject);
    	classroomSession.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.FALSE);
    	testCase.update(classroomSession);
    	assertEquals(3, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivision(classroomSessionDivision).size());
    	
    	classroomSession = inject(ClassroomSessionBusiness.class).findByLevelName(SchoolConstant.Code.LevelName.G1).iterator().next();
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.FALSE);
    	Student student = inject(StudentBusiness.class).instanciateOneRandomly("S001");
    	testCase.create(student);
    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S002");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	student = inject(StudentBusiness.class).instanciateOneRandomly("S003");
    	testCase.create(student);
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(classroomSession, student);
    	studentClassroomSession.setCode(student.getCode());
    	classroomSession.getStudents().addOne(studentClassroomSession);
    	classroomSession.getStudents().setSynchonizationEnabled(Boolean.TRUE);
    	testCase.update(classroomSession);
    	
    	assertEquals(0, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(inject(StudentDao.class).read("S002")
    			,classroomSessionDivision).size());
    	
    	studentClassroomSession = inject(StudentClassroomSessionBusiness.class).find("S002");
    	StudentClassroomSessionSubject studentClassroomSessionSubject = inject(StudentClassroomSessionSubjectBusiness.class).instanciateOne(studentClassroomSession
    			, inject(ClassroomSessionSubjectDao.class).readByClassroomSessionBySubject(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT)));
    	studentClassroomSession.getStudentClassroomSessionSubjects().addOne(studentClassroomSessionSubject);
    	studentClassroomSessionSubject = inject(StudentClassroomSessionSubjectBusiness.class).instanciateOne(studentClassroomSession
    			, inject(ClassroomSessionSubjectDao.class).readByClassroomSessionBySubject(classroomSession, inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS)));
    	studentClassroomSession.getStudentClassroomSessionSubjects().addOne(studentClassroomSessionSubject);
    	studentClassroomSession.getStudentClassroomSessionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	testCase.update(studentClassroomSession);
    	assertEquals(2, inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(inject(StudentDao.class).read("S002")
    			,classroomSessionDivision).size());
    	
    	testCase.clean();
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1);
    	
    	//dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	//dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
