package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;

public class IesaCrudBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    
    @Override
    protected void businesses() {
    	/*installApplication();
    	ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).find("PRIMARY_G1_YEAR_B");
    	Long classroomSessionDivisionSubjectCount1 = inject(ClassroomSessionDivisionSubjectBusiness.class).countAll();
    	ClassroomSessionDivision classroomSessionDivision1 = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession, 1l);
    	Long studentClassroomSessionDivisionSubjectCount1 = inject(StudentClassroomSessionDivisionSubjectBusiness.class).countAll();
    	ClassroomSessionDivision classroomSessionDivision2 = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession, 2l);
    	ClassroomSessionDivision classroomSessionDivision3 = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession, 3l);
    	Subject subject = inject(SubjectBusiness.class).find("Chemistry");
    	SubjectClassroomSession subjectClassroomSession = new SubjectClassroomSession(subject, classroomSession);	
    	create(subjectClassroomSession);
    	Long classroomSessionDivisionSubjectCount2 = inject(ClassroomSessionDivisionSubjectBusiness.class).countAll();
    	System.out.println(classroomSessionDivisionSubjectCount1+" - "+classroomSessionDivisionSubjectCount2+" : "+(classroomSessionDivisionSubjectCount2-classroomSessionDivisionSubjectCount1));
    	System.exit(0);
    	*/
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
