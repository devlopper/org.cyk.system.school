package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.BusinessInterfaceLocator;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;

public class IesaAutoPropertySetBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    
    @Override
    protected void businesses() {
    	installApplication();
    	print(LevelGroup.class);
    	print(LevelTimeDivision.class);
    	/*
    	for(Subject subject : inject(SubjectBusiness.class).findAll())
    		System.out.println("Subjects : "+subject.getCode()+" , "+subject.getName());
    	for(AcademicSession academicSession : inject(AcademicSessionBusiness.class).findAll())
    		System.out.println("Academic Session : "+academicSession.getCode()+" , "+academicSession.getName());
    	for(LevelGroup levelGroup : inject(LevelGroupBusiness.class).findAll())
    		System.out.println("Level Group : "+levelGroup.getCode()+" , "+levelGroup.getName());
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
    	*/	
    	System.exit(0);
    }
    
    private <IDENTIFIABLE extends AbstractIdentifiable> void print(Class<IDENTIFIABLE> aClass){
    	for(IDENTIFIABLE identifiable : inject(BusinessInterfaceLocator.class).injectTyped(aClass).findAll())
    		System.out.println(aClass.getSimpleName()+" : "+identifiable.getCode()+" , "+identifiable.getName());
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
