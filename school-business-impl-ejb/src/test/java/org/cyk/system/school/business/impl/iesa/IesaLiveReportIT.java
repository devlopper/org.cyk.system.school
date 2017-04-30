package org.cyk.system.school.business.impl.iesa;

import java.util.Locale;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.persistence.api.file.FileIdentifiableGlobalIdentifierDao;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.utility.common.generator.AbstractGeneratable;

public class IesaLiveReportIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT = Boolean.TRUE;
    	AbstractGeneratable.NULL_VALUE = "NA";
    	try{
    		File file = null;
    		/*file = inject(FileDao.class).read(SchoolConstant.Code.ReportTemplate.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS);
    		file.setBytes(IOUtils.toByteArray(SchoolBusinessLayer.class.getResourceAsStream("report/iesa/g1g12.jrxml")));
        	update(file);
        	
        	file = inject(FileDao.class).read(SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET);
    		file.setBytes(IOUtils.toByteArray(SchoolBusinessLayer.class.getResourceAsStream("report/iesa/broadsheet.jrxml")));
        	update(file);
    		*/
    		
        	ClassroomSessionDivision classroomSessionDivisionG1A1 = inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(inject(ClassroomSessionDao.class)
					.readByLevelNameBySuffix("G1","A").iterator().next(), 1l);
        	
    		inject(GenericBusiness.class).createReportFile(classroomSessionDivisionG1A1, SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET
        			, Locale.ENGLISH);
    		
    		file = inject(FileIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(classroomSessionDivisionG1A1).iterator().next().getFile();
        	schoolBusinessTestHelper.write(file);
    		
    		ClassroomSessionDivision classroomSessionDivisionG1A2 = inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(inject(ClassroomSessionDao.class)
					.readByLevelNameBySuffix("G1","A").iterator().next(), 2l);
    		
    		StudentClassroomSessionDivision studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionDao.class)
    				.readByClassroomSessionDivision(classroomSessionDivisionG1A2).iterator().next();
    		
    		file = inject(GenericBusiness.class).createReportFile(studentClassroomSessionDivision, SchoolConstant.Code.ReportTemplate.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS, Locale.ENGLISH);
    		//schoolBusinessTestHelper.write(file);
    		
    		inject(GenericBusiness.class).createReportFile(classroomSessionDivisionG1A2, SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET
        			, Locale.ENGLISH);
    		
    		file = inject(FileIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(classroomSessionDivisionG1A2).iterator().next().getFile();
        	schoolBusinessTestHelper.write(file);	
        	
        	/**/
        	
        	ClassroomSessionDivision classroomSessionDivisionG12 = inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(inject(ClassroomSessionDao.class)
					.readByLevelName("G12").iterator().next(), 2l);
    		
    		inject(GenericBusiness.class).createReportFile(classroomSessionDivisionG12, SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET
        			, Locale.ENGLISH);
    		
    		file = inject(FileIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(classroomSessionDivisionG12).iterator().next().getFile();
        	schoolBusinessTestHelper.write(file);	
        	
    	}catch(Exception exception){
    		exception.printStackTrace();
    	}
    	
    	System.exit(0);
    }
     
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer()
    			.setStructurationEnabled(Boolean.FALSE)
    			.setSynchronizationEnabled(Boolean.FALSE)
    			.setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getDivisionOrderNumbers().clear();
    	return dataProducer;
    }
        
}
