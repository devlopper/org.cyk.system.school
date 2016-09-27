package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.LevelBusiness;
import org.cyk.system.school.business.api.session.LevelTimeDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.SubjectBusiness;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Subject;

public class IesaAutoPropertySetBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    
    @Override
    protected void businesses() {
    	installApplication();
    	for(Subject subject : inject(SubjectBusiness.class).findAll())
    		System.out.println("Subjects : "+subject.getCode()+" , "+subject.getName());
    	for(AcademicSession academicSession : inject(AcademicSessionBusiness.class).findAll())
    		System.out.println("Academic Session : "+academicSession.getCode()+" , "+academicSession.getName());
    	for(Level level : inject(LevelBusiness.class).findAll())
    		System.out.println("Level : "+level.getCode()+" , "+level.getName());
    	for(LevelTimeDivision levelTimeDivision : inject(LevelTimeDivisionBusiness.class).findAll())
    		System.out.println("LevelTimeDivision : "+levelTimeDivision.getCode()+" , "+levelTimeDivision.getName());
    	for(ClassroomSession classroomSession : inject(ClassroomSessionBusiness.class).findAll())
    		System.out.println("ClassroomSession : "+classroomSession.getCode()+" , "+classroomSession.getName());
    	for(ClassroomSessionDivision classroomSessionDivision : inject(ClassroomSessionDivisionBusiness.class).findAll())
    		System.out.println("ClassroomSessionDivision : "+classroomSessionDivision.getCode()+" , "+classroomSessionDivision.getName());
    	for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectBusiness.class).findAll())
    		System.out.println("ClassroomSessionDivisionSubject : "+classroomSessionDivisionSubject.getCode()+" , "+classroomSessionDivisionSubject.getName());
    	for(EvaluationType evaluationType : inject(EvaluationTypeBusiness.class).findAll())
    		System.out.println("EvaluationType : "+evaluationType.getCode()+" , "+evaluationType.getName());
    	for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findAll())
    		System.out.println("ClassroomSessionDivisionSubjectEvaluationType : "+classroomSessionDivisionSubjectEvaluationType.getCode()+" , "+classroomSessionDivisionSubjectEvaluationType.getName());
    		
    	System.exit(0);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
