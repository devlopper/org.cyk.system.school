package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.LevelBusiness;
import org.cyk.system.school.business.api.session.LevelTimeDivisionBusiness;
import org.cyk.system.school.business.api.session.SubjectClassroomSessionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.SubjectBusiness;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;

public class IesaCrudBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    
    @Override
    protected void businesses() {
    	installApplication();
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
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
