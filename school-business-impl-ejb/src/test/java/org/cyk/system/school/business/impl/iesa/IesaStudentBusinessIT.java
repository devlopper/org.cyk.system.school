package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.LevelTimeDivisionBusiness;
import org.cyk.system.school.model.actor.Student;

public class IesaStudentBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
      
    @Override
    protected void businesses() {
    	installApplication();
    	Student student = inject(StudentBusiness.class).instanciateOne();
    	student.setName("Zeze");
    	student.getPerson().setLastnames("Marius");
    	student.setAdmissionLevelTimeDivision(inject(LevelTimeDivisionBusiness.class).find("PRIMARY_G1_YEAR"));
    	create(student);
    	assertEquals("IESA/2016ZM0001-PRIMARY", student.getCode());
    	/*
    	inject(StudentBusiness.class).createReportFile(student, new CreateReportFileArguments<Student>(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE, student));
    	
    	FileIdentifiableGlobalIdentifier.SearchCriteria searchCriteria = new FileIdentifiableGlobalIdentifier.SearchCriteria();
    	searchCriteria.addIdentifiableGlobalIdentifier(student);
    	searchCriteria.addRepresentationType(inject(FileRepresentationTypeDao.class).read(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE));
    	Collection<FileIdentifiableGlobalIdentifier> fileIdentifiableGlobalIdentifiers = inject(FileIdentifiableGlobalIdentifierDao.class).readByCriteria(searchCriteria);
    	assertEquals(1, fileIdentifiableGlobalIdentifiers.size());
    	inject(RootBusinessTestHelper.class).write(fileIdentifiableGlobalIdentifiers.iterator().next().getFile());
    	
    	student = inject(StudentBusiness.class).find("EMP001");
    	student.setName("Komenan");
    	student.getPerson().setLastnames("Yao christian");
    	inject(StudentBusiness.class).update(student);
    	
    	inject(StudentBusiness.class).createReportFile(student, new CreateReportFileArguments<Student>(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE, student));
    	
    	searchCriteria = new FileIdentifiableGlobalIdentifier.SearchCriteria();
    	searchCriteria.addIdentifiableGlobalIdentifier(student);
    	searchCriteria.addRepresentationType(inject(FileRepresentationTypeDao.class).read(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE));
    	fileIdentifiableGlobalIdentifiers = inject(FileIdentifiableGlobalIdentifierDao.class).readByCriteria(searchCriteria);
    	assertEquals(1, fileIdentifiableGlobalIdentifiers.size());
    	inject(RootBusinessTestHelper.class).write(fileIdentifiableGlobalIdentifiers.iterator().next().getFile());
    	*/
    }
    
}
