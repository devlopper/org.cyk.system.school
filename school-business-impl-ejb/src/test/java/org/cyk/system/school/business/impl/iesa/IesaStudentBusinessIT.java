package org.cyk.system.school.business.impl.iesa;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.impl.RootBusinessTestHelper;
import org.cyk.system.root.model.file.FileIdentifiableGlobalIdentifier;
import org.cyk.system.root.persistence.api.file.FileIdentifiableGlobalIdentifierDao;
import org.cyk.system.root.persistence.api.file.FileRepresentationTypeDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;

public class IesaStudentBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
      
    @Override
    protected void businesses() {
    	installApplication();
    	Student student = inject(StudentBusiness.class).instanciateOne();
    	student.setCode("IESA_45_10");
    	student.setName("Zeze");
    	student.getPerson().setLastnames("Marius");
    	create(student);
    	
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
    }
    
}
