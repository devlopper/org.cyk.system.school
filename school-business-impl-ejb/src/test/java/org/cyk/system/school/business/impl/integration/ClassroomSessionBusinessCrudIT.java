package org.cyk.system.school.business.impl.integration;

import java.util.Arrays;

import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSession.SearchCriteria;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.utility.common.CommonUtils;
import org.cyk.utility.common.CommonUtils.Execution;

public class ClassroomSessionBusinessCrudIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;

    @Override
	protected void populate() {
    	installApplication();
    	/*School school = new School(inject(OwnedCompanyBusiness.class).findDefaultOwnedCompany(),null);
    	create(school);
    	
    	final AcademicSession academicSession = new AcademicSession(school,null,new Date());
    	academicSession.getGlobalIdentifierCreateIfNull().getExistencePeriod().setFromDate(new Date());
    	academicSession.getExistencePeriod().setToDate(new Date(academicSession.getExistencePeriod().getFromDate().getTime()+DateTimeConstants.MILLIS_PER_DAY*355));
    	create(academicSession);
    	System.out.println(inject(LevelTimeDivisionDao.class).readAll());
    	create(new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
    			,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1), null));
    	create(new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
    			,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1), null));
    	create(new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
    			,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1), null));
    	create(new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
    			,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1), null));*/
	}
    
    //@Test
    public void create24OneByOne(){
    	for(int i = 0; i<1;i++){
    		final String suffix = i+"";
    		Execution execution = CommonUtils.getInstance().execute("Create Classroom session G1"+suffix, new Runnable() {
    			@Override
    			public void run() {
    				ClassroomSession classroomSession = new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
    						,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1), null,null);
    				//classroomSession.setSuffix(suffix);
    				create(classroomSession);
    			}
    		});
        	System.out.println(execution);	
    	}
    	System.out.println("ClassroomSession : "+inject(ClassroomSessionBusiness.class).countAll());
    	System.out.println("ClassroomSessionDivision : "+inject(ClassroomSessionDivisionBusiness.class).countAll());
    	System.out.println("ClassroomSessionDivisionSubject : "+inject(ClassroomSessionDivisionSubjectBusiness.class).countAll());
    	System.out.println("ClassroomSessionDivisionSubjectEvaluationType : "+inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).countAll());
    	
    }
    
    //@Test
    public void create24UsingThread(){
    	
    }

    //@Test
    public void exceptionDuplicate(){
    	new org.cyk.utility.common.test.TestEnvironmentListener.Try("Un enregistrement avec pour code = abc existe déja"){ 
			private static final long serialVersionUID = -8176804174113453706L;
			@Override protected void code() {
				create(new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
						,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1), null,null));
			}
		}.execute();
    }
    
    //@Test
    public void searchCriteria(){
    	SearchCriteria searchCriteria = new SearchCriteria();
    	searchCriteria.addLevelTimeDivisions(Arrays.asList(inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1)));
    	
    	System.out.println(inject(ClassroomSessionBusiness.class).findByCriteria(searchCriteria));
    	
    }
}
