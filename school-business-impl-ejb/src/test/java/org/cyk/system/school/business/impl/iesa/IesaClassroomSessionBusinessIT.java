package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;
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
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
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
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
    	classroomSession.getDivisions().addOne(classroomSessionDivision1 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision2 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision3 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l));
    	classroomSessionDivision1.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision1.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision1,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivision2.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision2.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision2,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS)));
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY)));
    	
    	testCase.create(classroomSession);
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(4, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	assertEquals(3, inject(SubjectClassroomSessionDao.class).readByClassroomSession(classroomSession).size());
    	testCase.clean();
    }
    
    @Test
    public void crudClassroomAndDivisionsAndSubjectAndEvaluationType(){
    	TestCase testCase = instanciateTestCase();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).instanciateOne();
    	classroomSession.getNodeInformations().setClassroomSessionTimeDivisionType(inject(TimeDivisionTypeDao.class).read(RootConstant.Code.TimeDivisionType.TRIMESTER));
    	classroomSession.setLevelTimeDivision(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1));
    	classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
    	classroomSession.getDivisions().addOne(classroomSessionDivision1 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,1l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision2 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,2l));
    	classroomSession.getDivisions().addOne(classroomSessionDivision3 = inject(ClassroomSessionDivisionBusiness.class).instanciateOne(classroomSession,3l));
    	classroomSessionDivision1.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
    	classroomSessionDivision1.getClassroomSessionDivisionSubjects().addOne(classroomSessionDivisionSubject = inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision1,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ACCOUNTING)));
    	classroomSessionDivisionSubject.getEvaluationTypes().addOne(inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).instancaiteOne(classroomSessionDivisionSubject
    			, inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST1)));
    	classroomSessionDivision2.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision2.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision2,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS)));
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.ART_CRAFT)));
    	classroomSessionDivision3.getClassroomSessionDivisionSubjects().addOne(inject(ClassroomSessionDivisionSubjectBusiness.class)
    			.instanciateOne(classroomSessionDivision3,inject(SubjectDao.class).read(SchoolConstant.Code.Subject.BIOLOGY)));
    	
    	testCase.create(classroomSession);
    	assertEquals(3l, inject(ClassroomSessionDivisionDao.class).countByClassroomSession(classroomSession));
    	assertEquals(4, inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSession(classroomSession).size());
    	testCase.clean();
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	
    	//dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	//dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
