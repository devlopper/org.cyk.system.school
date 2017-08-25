package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.root.business.api.ClazzBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.network.UniformResourceLocatorBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFormatter;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.DataSet;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.ContentType;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionType;
import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.root.model.security.Role;
import org.cyk.system.root.model.value.Value;
import org.cyk.system.root.model.value.ValueCollection;
import org.cyk.system.root.model.value.ValueCollectionItem;
import org.cyk.system.root.model.value.ValueProperties;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelSpeciality;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER) @Getter
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Setter private AverageComputationListener averageComputationListener;
	@Setter private Script averageComputationScript;
	
	private String actionCreateSubjectEvaluation = "acse";
	private String actionAssignSubjectClassroomSessionToStudentClassroomSession = "aascstscs";
	private String actionUpdateStudentClassroomSessionDivisionResults = "auscsdr";
	private String actionComputeStudentClassroomSessionDivisionEvaluationResults = "acscsder";
	private String actionComputeStudentClassroomSessionEvaluationResults = "acscser";
	private String actionComputeStudentClassroomSessionDivisionRankResults = "acscsdrr";
	private String actionComputeStudentClassroomSessionDivisionAttendanceResults = "acscsdar";
	private String actionUpdateStudentClassroomSessionDivisionReportFiles = "auscsdrf";
	private String actionSendStudentClassroomSessionDivisionReportFiles = "asscsdrf";
	private String actionConsultStudentClassroomSessionDivisionReportFiles = "acscsdrf";
	private String actionConsultStudentClassroomSessionRanks = "acscsr";
	
	
	private String actionEditStudentClassroomSessionDivisionEvaluationAverage = "aescsdea";
	private String actionUpdateClassroomSessionDivisionBroadsheet = "aucsdbs";
	private String actionConsultClassroomSessionDivisionBroadsheet = "accsdbs";
	
	private RankOptions<SortableStudentResults> studentEvaluationResultsRankOptions = new RankOptions<>();
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
		AbstractSchoolReportProducer.DEFAULT = new AbstractSchoolReportProducer.Default();
		PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				if(ArrayUtils.contains(new String[]{CompanyConstant.Code.ReportTemplate.EMPLOYEE_EMPLOYMENT_CONTRACT}, instanceCode)){
					if(PersistDataListener.BASE_PACKAGE.equals(name))
						return (T) CompanyBusinessLayer.class.getPackage();
				}
				return super.processPropertyValue(aClass, instanceCode, name, value);
			}
		});
		
		AbstractRootReportProducer.DEFAULT = new AbstractSchoolReportProducer.Default();
		RootBusinessLayer.GLOBAL_IDENTIFIER_UNBUILDABLE_CLASSES.add(StudentResults.class);
		registerFormatter(AcademicSession.class, new AbstractFormatter<AcademicSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(AcademicSession academicSession, ContentType contentType) {
				return inject(TimeBusiness.class).formatDate(academicSession.getExistencePeriod().getFromDate(), academicSession.getExistencePeriod().getToDate(), TimeBusiness.DATE_SHORT_PATTERN) ;
			}
		});
		registerFormatter(ClassroomSession.class, new AbstractFormatter<ClassroomSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSession classroomSession, ContentType contentType) {
				return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getName()
						+(classroomSession.getSuffix()==null?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE
								+formatterBusiness.format(classroomSession.getSuffix(),contentType));
			}
		});
		registerFormatter(ClassroomSessionDivision.class, new AbstractFormatter<ClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSessionDivision classroomSessionDivision, ContentType contentType) {
				return classroomSessionDivision.getName();//+Constant.CHARACTER_SPACE+(classroomSessionDivision.getOrderNumber());
			}
		});
		
		registerFormatter(StudentClassroomSessionDivision.class, new AbstractFormatter<StudentClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(StudentClassroomSessionDivision studentClassroomSessionDivision, ContentType contentType) {
				//return inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getStudent(),contentType)
				//		+Constant.CHARACTER_SPACE+;
				
				//return studentClassroomSessionDivision.getName();
				
				//return inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getClassroomSessionDivision(),contentType)+Constant.CHARACTER_SPACE
				//		+inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getStudent(),contentType);
				
				ClassroomSession classroomSession = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession();
				
				return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getName()
						+(classroomSession.getSuffix()==null?Constant.EMPTY_STRING:classroomSession.getSuffix().getName())
						+Constant.CHARACTER_SPACE+studentClassroomSessionDivision.getStudent().getCode()+Constant.CHARACTER_SPACE
						+studentClassroomSessionDivision.getStudent().getPerson().getNames();
			}
		});
		
		ClazzBusiness.LISTENERS.add(new ClazzBusiness.ClazzBusinessListener.Adapter(){
			private static final long serialVersionUID = -6563167908087619179L;
			@Override
			public Object getParentOf(Object object) {
				if(object instanceof AbstractIdentifiable){
					AbstractIdentifiable identifiable = (AbstractIdentifiable) object;
					if(identifiable instanceof ClassroomSession)
						return null;//((ClassroomSession)object).getAcademicSession();
					if(identifiable instanceof ClassroomSessionSubject)
						return ((ClassroomSessionSubject)identifiable).getClassroomSession();
					
					if(identifiable instanceof ClassroomSessionDivision)
						return ((ClassroomSessionDivision)identifiable).getClassroomSession();
					if(identifiable instanceof ClassroomSessionDivisionSubject)
						return ((ClassroomSessionDivisionSubject)identifiable).getClassroomSessionDivision();
					if(identifiable instanceof ClassroomSessionDivisionSubjectEvaluationType)
						return ((ClassroomSessionDivisionSubjectEvaluationType)identifiable).getClassroomSessionDivisionSubject();
					
					if(identifiable instanceof Evaluation)
						return ((Evaluation)identifiable).getClassroomSessionDivisionSubjectEvaluationType();
					
					if(identifiable instanceof StudentClassroomSession)
						return ((StudentClassroomSession)identifiable).getClassroomSession();
					if(identifiable instanceof StudentClassroomSessionDivision)
						return ((StudentClassroomSessionDivision)identifiable).getClassroomSessionDivision();
					if(identifiable instanceof StudentClassroomSessionDivisionSubject)
						return ((StudentClassroomSessionDivisionSubject)identifiable).getClassroomSessionDivisionSubject();
				}
				return super.getParentOf(object);
			}
		});
		
		studentEvaluationResultsRankOptions.setType(RankType.EXAEQUO);
		studentEvaluationResultsRankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
		
		inject(GlobalIdentifierPersistenceMappingConfigurations.class).configure();
		
		AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}
		,AcademicSession.class,Level.class,LevelTimeDivision.class,ClassroomSession.class,ClassroomSessionSubject.class,ClassroomSessionDivision.class
		,ClassroomSessionDivisionSubject.class,ClassroomSessionDivisionSubjectEvaluationType.class,StudentClassroomSession.class,StudentClassroomSessionDivision.class
		,StudentClassroomSessionDivisionSubject.class,StudentClassroomSessionSubject.class,Evaluation.class);
        
	}
	
	@Override
	protected AbstractReportRepository getReportRepository() {
		return inject(SchoolReportRepository.class);
	}
	
	@Override
	protected void persistStructureData() {
		DataSet dataSet = new DataSet(getClass());
		
		createFiles(dataSet);
    	createIntervals(dataSet);
    	createValues(dataSet);
    	
    	createSubjects(dataSet);
    	createMetricColletions(dataSet);
    	createLevels(dataSet);
    	createEvaluations(dataSet);
    	
    	createSchool(dataSet);
    	
    	dataSet.instanciate();
    	dataSet.save();
    
	}
	
	private void createFiles(DataSet dataSet){
		dataSet.addClass(File.class);
		dataSet.addClass(Script.class);
		dataSet.addClass(ReportTemplate.class);
		
	}
	
	private void createValues(DataSet dataSet){
		dataSet.addClass(ValueProperties.class);
		dataSet.addClass(Value.class);
		dataSet.addClass(ValueCollection.class);
		dataSet.addClass(ValueCollectionItem.class);
	}
	
	private void createSchool(DataSet dataSet){
		dataSet.addClass(School.class);
		dataSet.addClass(AcademicSession.class);
	}
	
	private void createLevels(DataSet dataSet){
		dataSet.addClass(LevelGroupType.class);
		dataSet.addClass(LevelGroup.class);
		dataSet.addClass(LevelSpeciality.class);
		
		dataSet.addClass(LevelName.class);
		dataSet.addClass(Level.class);
		dataSet.addClass(LevelTimeDivision.class);
		dataSet.addClass(ClassroomSessionSuffix.class);
	}
	
	private void createIntervals(DataSet dataSet){
		dataSet.addClass(IntervalCollection.class);
		dataSet.addClass(Interval.class);
	}
	
	private void createEvaluations(DataSet dataSet){
		dataSet.addClass(EvaluationType.class);
	}
	
	private void createMetricColletions(DataSet dataSet){
		dataSet.addClass(MetricCollectionType.class);
		dataSet.addClass(MetricCollection.class);
		dataSet.addClass(Metric.class);
	}
	
	private void createSubjects(DataSet dataSet){
		dataSet.addClass(Subject.class);
	}
	
	@Override
	protected void persistSecurityData(){
		UniformResourceLocatorBusiness uniformResourceLocatorBusiness = inject(UniformResourceLocatorBusiness.class);
		Role userRole = getEnumeration(Role.class,RootConstant.Code.Role.USER);
		Role managerRole = getEnumeration(Role.class,RootConstant.Code.Role.MANAGER);
		Role teacherRole = create(new Role(SchoolConstant.Code.Role.TEACHER, "Teacher"));
		
		instanciateRoleUniformResourceLocator(userRole, uniformResourceLocatorBusiness.instanciateOne("/private/index.jsf",new String[]{})
				,uniformResourceLocatorBusiness.instanciateOne("/private/file/consultmany.jsf",new String[]{})
				/*,uniformResourceLocatorBusiness.instanciateOne("/private/classroomsessiondivision/consult.jsf"
						,new String[]{UniformResourceLocatorParameter.TAB_ID,"school.broadsheet" })
				*/
				,uniformResourceLocatorBusiness.instanciateOne("/private/classroomsessiondivision/consult.jsf"
						,new String[]{})
				,uniformResourceLocatorBusiness.instanciateOne("/private/__dynamic__/select/selectone.jsf"
						,new String[]{UniformResourceLocatorParameter.ACTION_IDENTIFIER,"auscsdr",UniformResourceLocatorParameter.CLASS,"ClassroomSessionDivision" })
				,uniformResourceLocatorBusiness.instanciateOne("/private/__dynamic__/select/selectone.jsf"
						,new String[]{UniformResourceLocatorParameter.ACTION_IDENTIFIER,"accsdbs",UniformResourceLocatorParameter.CLASS,"ClassroomSessionDivision" })
			);
		
		instanciateRoleUniformResourceLocator(managerRole,uniformResourceLocatorBusiness.instanciateManyCrud(Student.class)
				,uniformResourceLocatorBusiness.instanciateManyCrud(Teacher.class)
				,uniformResourceLocatorBusiness.instanciateManyCrud(Employee.class)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSession.class, Boolean.TRUE, Boolean.TRUE, Boolean.TRUE,null, null, new String[]{"auscsdrf","acscsdrf"})
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivision.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivisionSubject.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, new String[]{"acse","auscsdr"}, null)
				);
		
		instanciateRoleUniformResourceLocator(teacherRole
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSession.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,Boolean.TRUE, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSessionDivision.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSessionDivisionSubject.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,Boolean.TRUE, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(Evaluation.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null));
		
		instanciateUserAccountsFromActors(inject(TeacherDao.class).readAll(), userRole,teacherRole);
		
	}
	
	@Override
	public void enableEnterpriseResourcePlanning() {
		StudentBusinessImpl.Listener.COLLECTION.add(new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
    	TeacherBusinessImpl.Listener.COLLECTION.add(new TeacherBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
	}

	@Override
	protected void setConstants() {}
	
	/**/

	/**/
	
    public static SchoolBusinessLayer getInstance() {
		return INSTANCE;
	}
		
}
