package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER)
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Inject @Getter private TeacherBusiness teacherBusiness;
	@Inject @Getter private StudentBusiness studentBusiness;
	@Inject @Getter private StudentSubjectBusiness studentSubjectBusiness;
	@Inject @Getter private StudentSubjectEvaluationBusiness studentSubjectEvaluationBusiness;
	@Inject @Getter private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject @Getter private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
	@Inject @Getter private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject @Getter private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject @Getter private ClassroomSessionDivisionSubjectBusiness classroomSessionDivisionSubjectBusiness;
	@Inject @Getter private SubjectEvaluationBusiness subjectEvaluationBusiness;
	@Inject @Getter private SubjectEvaluationTypeBusiness subjectEvaluationTypeBusiness;
	@Inject @Getter private LectureBusiness lectureBusiness;
	
	@Getter @Setter private AverageComputationListener averageComputationListener;
	@Getter @Setter private Script averageComputationScript;
	@Getter @Setter private SchoolReportProducer reportProducer;
	@Inject private SchoolReportRepository schoolReportRepository;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
	}
	
	@Override
	protected AbstractReportRepository getReportRepository() {
		return schoolReportRepository;
	}
	
	@Override
	protected void persistData() {}

	@Override
	protected void setConstants() {}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void registerTypedBusinessBean(Map<Class<AbstractIdentifiable>, TypedBusiness<AbstractIdentifiable>> beansMap) {
        beansMap.put((Class)Student.class, (TypedBusiness)studentBusiness);
        beansMap.put((Class)Teacher.class, (TypedBusiness)teacherBusiness);
        beansMap.put((Class)ClassroomSession.class, (TypedBusiness)classroomSessionBusiness);
        beansMap.put((Class)ClassroomSessionDivision.class, (TypedBusiness)classroomSessionDivisionBusiness);
        beansMap.put((Class)ClassroomSessionDivisionSubject.class, (TypedBusiness)classroomSessionDivisionSubjectBusiness);
        beansMap.put((Class)SubjectEvaluation.class, (TypedBusiness)subjectEvaluationBusiness);
        beansMap.put((Class)SubjectEvaluationType.class, (TypedBusiness)subjectEvaluationTypeBusiness);
        beansMap.put((Class)Lecture.class, (TypedBusiness)lectureBusiness);
    }

	@Override
	protected void fakeTransactions() {}
	    
    public static SchoolBusinessLayer getInstance() {
		return INSTANCE;
	}
		
}
