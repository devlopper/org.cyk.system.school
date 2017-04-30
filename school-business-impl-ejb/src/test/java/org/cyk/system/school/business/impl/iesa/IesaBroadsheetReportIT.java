package org.cyk.system.school.business.impl.iesa;

import java.util.Collection;
import java.util.Locale;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.persistence.api.file.FileIdentifiableGlobalIdentifierDao;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.generator.RandomDataProvider;

public class IesaBroadsheetReportIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
        
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	Collection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class).readAll();
    	for(StudentClassroomSessionDivisionSubject index : studentClassroomSessionDivisionSubjects)
    		if(RandomDataProvider.getInstance().randomBoolean())
    			inject(StudentClassroomSessionDivisionSubjectBusiness.class).delete(index);
    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport( ((IesaFakedDataProducer)getFakedDataProducer()).generate()
    			, new Boolean[]{Boolean.FALSE},Boolean.TRUE, Boolean.FALSE);
    	/*
    	ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class)
    			.findByLevelTimeDivisionCodeByClassroomSessionSuffixCodeByClassroomSessionDivisionOrderNumber(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1
    					,SchoolConstant.Code.ClassroomSessionSuffix.A,2l).iterator().next();
    	*/
    	
    	ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class)
    			.findByLevelTimeDivisionCodeByClassroomSessionDivisionOrderNumber(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1,2l).iterator().next();
    	
    	inject(GenericBusiness.class).createReportFile(classroomSessionDivision, SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET
    			, Locale.ENGLISH);
    	
    	File file = inject(FileIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(classroomSessionDivision).iterator().next().getFile();
    	schoolBusinessTestHelper.write(file);
    	
    	
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	//dataProducer.getClassroomSessionSuffixes().clear();
    	
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	
    	/*
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	*/
    	
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1);
    	//dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	
    	//dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	//dataProducer.getDivisionOrderNumbers().add(1l);
    	dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
