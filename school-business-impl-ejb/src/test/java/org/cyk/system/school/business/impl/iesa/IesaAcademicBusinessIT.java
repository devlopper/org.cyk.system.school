package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.junit.Test;

public class IesaAcademicBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
        
    @Override
    protected void businesses() {
    
    }
    
    @Test
    public void getSchoolDefaulted(){
    	assertThat("default does not exist", inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted()!=null);
    }
    
    @Test
    public void updateCascade(){
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	
    	AcademicSession academicSession = inject(AcademicSessionDao.class).readOneRandomly();
    	assertEquals(1l, academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
    	assertEquals(1l, inject(LevelNameDao.class).readOneRandomly().getNodeInformations().getCurrentClassroomSessionDivisionIndex());
    	
    	academicSession.getNodeInformations().setCurrentClassroomSessionDivisionIndex(2l);
    	academicSession.getLevelGroups().setSynchonizationEnabled(Boolean.TRUE);
    	academicSession.getLevelGroups().addManyFieldName(CommonNodeInformations.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX);
    	
    	academicSession.getLevelNames().setSynchonizationEnabled(Boolean.TRUE);
    	academicSession.getLevelNames().addMany(inject(LevelNameDao.class).readAll());
    	academicSession.getLevelNames().addManyFieldName(CommonNodeInformations.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX);
    	update(academicSession);
    	
    	academicSession = inject(AcademicSessionDao.class).read(academicSession.getIdentifier());
    	assertEquals(2l, academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
    	assertEquals(2l, inject(LevelNameDao.class).readOneRandomly().getNodeInformations().getCurrentClassroomSessionDivisionIndex());
    }
    /*
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	
    	dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	//dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
    */
        
}
