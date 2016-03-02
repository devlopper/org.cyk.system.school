package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.api.ClazzBusiness;
import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFormatter;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.ContentType;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionStudentsMetricCollectionBusiness;
import org.cyk.system.school.business.api.session.EvaluationTypeBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.session.LevelGroupTypeBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER) @Getter
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Inject private AcademicSessionBusiness academicSessionBusiness;
	@Inject private TeacherBusiness teacherBusiness;
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentResultsMetricValueBusiness studentResultsMetricValueBusiness;
	@Inject private StudentSubjectEvaluationBusiness studentSubjectEvaluationBusiness;
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private ClassroomSessionDivisionSubjectBusiness classroomSessionDivisionSubjectBusiness;
	@Inject private ClassroomSessionDivisionStudentsMetricCollectionBusiness classroomSessionDivisionStudentsMetricCollectionBusiness;
	@Inject private LevelGroupTypeBusiness levelGroupTypeBusiness;
	@Inject private EvaluationBusiness evaluationBusiness;
	@Inject private EvaluationTypeBusiness evaluationTypeBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeBusiness subjectEvaluationTypeBusiness;
	@Inject private LectureBusiness lectureBusiness;
	@Inject private LevelGroupBusiness levelGroupBusiness;
	@Inject private SubjectClassroomSessionBusiness subjectClassroomSessionBusiness;
	
	@Setter private AverageComputationListener averageComputationListener;
	@Setter private Script averageComputationScript;
	@Setter private SchoolReportProducer reportProducer;
	@Inject private SchoolReportRepository schoolReportRepository;
	
	private String actionCreateSubjectEvaluation = "acse";
	private String actionUpdateStudentClassroomSessionDivisionResults = "auscsdr";
	private String actionUpdateStudentClassroomSessionDivisionReportFiles = "auscsdrf";
	private String actionConsultStudentClassroomSessionDivisionReportFiles = "acscsdrf";
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
		registerFormatter(AcademicSession.class, new AbstractFormatter<AcademicSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(AcademicSession academicSession, ContentType contentType) {
				return RootBusinessLayer.getInstance().getTimeBusiness().formatDate(academicSession.getPeriod().getFromDate(), academicSession.getPeriod().getToDate(), TimeBusiness.DATE_SHORT_PATTERN) ;
			}
		});
		registerFormatter(ClassroomSession.class, new AbstractFormatter<ClassroomSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSession classroomSession, ContentType contentType) {
				return classroomSession.getLevelTimeDivision().getLevel().getName().getName()
						+(StringUtils.isBlank(classroomSession.getSuffix())?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE+classroomSession.getSuffix());
			}
		});
		registerFormatter(ClassroomSessionDivision.class, new AbstractFormatter<ClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSessionDivision classroomSessionDivision, ContentType contentType) {
				return classroomSessionDivision.getTimeDivisionType().getUiString()+" "+(classroomSessionDivision.getIndex()+1);
			}
		});
		
		ClazzBusiness.LISTENERS.add(new ClazzBusiness.ClazzBusinessListener.Adapter(){
			private static final long serialVersionUID = -6563167908087619179L;
			@Override
			public Object getParentOf(Object object) {
				
				if(object instanceof ClassroomSession)
					return null;//((ClassroomSession)object).getAcademicSession();
				if(object instanceof ClassroomSessionDivision)
					return ((ClassroomSessionDivision)object).getClassroomSession();
				if(object instanceof ClassroomSessionDivisionSubject)
					return ((ClassroomSessionDivisionSubject)object).getClassroomSessionDivision();
				if(object instanceof ClassroomSessionDivisionSubjectEvaluationType)
					return ((ClassroomSessionDivisionSubjectEvaluationType)object).getClassroomSessionDivisionSubject();
				
				if(object instanceof StudentClassroomSessionDivision)
					return ((StudentClassroomSessionDivision)object).getClassroomSessionDivision();
				
				return super.getParentOf(object);
			}
		});
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
        beansMap.put((Class)Evaluation.class, (TypedBusiness)evaluationBusiness);
        beansMap.put((Class)ClassroomSessionDivisionSubjectEvaluationType.class, (TypedBusiness)subjectEvaluationTypeBusiness);
        beansMap.put((Class)Lecture.class, (TypedBusiness)lectureBusiness);
        beansMap.put((Class)StudentClassroomSession.class, (TypedBusiness)studentClassroomSessionBusiness);
        beansMap.put((Class)StudentClassroomSessionDivision.class, (TypedBusiness)studentClassroomSessionDivisionBusiness);
        beansMap.put((Class)StudentSubject.class, (TypedBusiness)studentClassroomSessionDivisionBusiness);
        beansMap.put((Class)LevelGroup.class, (TypedBusiness)levelGroupBusiness);
    }
	
	

	@Override
	protected void fakeTransactions() {}
	
	/**/
	

	
	/**/
	
    public static SchoolBusinessLayer getInstance() {
		return INSTANCE;
	}
		
}
