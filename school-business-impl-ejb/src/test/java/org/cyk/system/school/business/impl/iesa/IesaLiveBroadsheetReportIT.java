package org.cyk.system.school.business.impl.iesa;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.persistence.api.file.FileDao;
import org.cyk.system.root.persistence.api.file.FileIdentifiableGlobalIdentifierDao;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSessionDivision;

public class IesaLiveBroadsheetReportIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	File file = inject(FileDao.class).read(SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET);
    	try {
			file.setBytes(IOUtils.toByteArray(SchoolBusinessLayer.class.getResourceAsStream("report/iesa/broadsheet.jrxml")));
		} catch (IOException e) {
			e.printStackTrace();
		}
    	update(file);
    	
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>();
    	classroomSessionDivisions.add(inject(ClassroomSessionDivisionBusiness.class)
    			.findByLevelTimeDivisionCodeByClassroomSessionSuffixCodeByClassroomSessionDivisionOrderNumber(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.ClassroomSessionSuffix.A,2l).iterator().next());
    	
    	for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
    		inject(GenericBusiness.class).createReportFile(classroomSessionDivision, SchoolConstant.Code.ReportTemplate.CLASSROOM_SESSION_DIVISION_BROAD_SHEET
        			, Locale.ENGLISH);
        	
        	file = inject(FileIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(classroomSessionDivision).iterator().next().getFile();
        	schoolBusinessTestHelper.write(file);	
    	}
    	
    	System.exit(0);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer()
    			.setStructurationEnabled(Boolean.FALSE)
    			.setSynchronizationEnabled(Boolean.FALSE)
    			.setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(2l);
    	return dataProducer;
    }
        
}
