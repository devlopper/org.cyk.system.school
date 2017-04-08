package org.cyk.system.school.business.impl.iesa;

import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.root.business.api.message.MailBusiness;
import org.cyk.system.root.business.api.message.SmtpPropertiesBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.message.SmtpPropertiesDao;
import org.cyk.system.root.model.message.SmtpProperties;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.LevelGroupDao;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class IesaStudentClassroomSessionCrudBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void installApplication() {
    	super.installApplication();
    	SmtpProperties smtpProperties = inject(SmtpPropertiesDao.class).read(RootConstant.Code.SmtpProperties.DEFAULT);
    	smtpProperties.setHost("smtp.gmail.com");
    	smtpProperties.setFrom("iesaciresults@gmail.com");
    	smtpProperties.setPort(465);
    	smtpProperties.getCredentials().setUsername("iesaciresults@gmail.com");
    	smtpProperties.getCredentials().setPassword("17abIESAresults");
    	update(smtpProperties);
    	
    }
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();  
    	
    	create(inject(TeacherBusiness.class).instanciateManyRandomly(5));
    	
    	Collection<ClassroomSession> classroomSessions = new ArrayList<>();
    	for(ClassroomSession classroomSession : inject(ClassroomSessionDao.class).readAll())
    		if(classroomSession.getCoordinator()==null){
    			classroomSession.setCoordinator(inject(TeacherBusiness.class).findOneRandomly());
    			classroomSessions.add(classroomSession);
    		}
    	update(classroomSessions);
    	
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>();
    	for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectDao.class).readAll())
    		if(classroomSessionDivisionSubject.getTeacher()==null){
    			classroomSessionDivisionSubject.setTeacher(inject(TeacherBusiness.class).findOneRandomly());
    			classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
    		}
    	update(classroomSessionDivisionSubjects);
    	
    	Collection<LevelGroup> levelGroups = new ArrayList<>();
    	for(LevelGroup levelGroup : inject(LevelGroupDao.class).readAll())
    		if(levelGroup.getNodeInformations().getStudentClassroomSessionDivisionResultsReportSigner()==null){
    			levelGroup.getNodeInformations().setStudentClassroomSessionDivisionResultsReportSigner(inject(PersonBusiness.class).findOneRandomly());
    			levelGroups.add(levelGroup);
    		}
    	update(levelGroups);
    	
    	LevelGroup levelGroup = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.KINDERGARTEN);
    	levelGroup.setName("Kindergarten");
    	update(levelGroup);
    	
    	LevelName levelName = inject(LevelNameDao.class).read(SchoolConstant.Code.LevelName.PK);
    	levelName.setName("Pre-K");
    	update(levelName);
    	PersonBusiness.FindNamesArguments.FIRST_NAME_IS_FIRST = Boolean.FALSE;
    	
    	//inject(MailBusiness.class).setProperties("iesa-ci.com", 465, "results@iesa-ci.com", "17abIESAresults");
    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport( ((IesaFakedDataProducer)getFakedDataProducer()).generate()
    			, new Boolean[]{Boolean.FALSE},Boolean.FALSE, Boolean.TRUE);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	/*dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1);
    	*///dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	//dataProducer.getDivisionOrderNumbers().add(1l);
    	dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
