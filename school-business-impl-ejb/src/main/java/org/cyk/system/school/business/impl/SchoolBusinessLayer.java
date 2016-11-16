package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.product.IntangibleProductBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyDataProducerHelper;
import org.cyk.system.company.model.product.IntangibleProduct;
import org.cyk.system.company.model.sale.SalableProduct;
import org.cyk.system.root.business.api.ClazzBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFormatter;
import org.cyk.system.root.business.impl.BusinessServiceProvider;
import org.cyk.system.root.business.impl.BusinessServiceProvider.Service;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.ContentType;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;
import org.cyk.utility.common.computation.DataReadConfiguration;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER) @Getter
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Setter private AverageComputationListener averageComputationListener;
	@Setter private Script averageComputationScript;
	
	private String actionCreateSubjectEvaluation = "acse";
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
	private String actionConsultClassroomSessionDivisionBroadsheet = "accsdbs";
	
	private RankOptions<SortableStudentResults> studentEvaluationResultsRankOptions = new RankOptions<>();
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
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
						+(StringUtils.isBlank(classroomSession.getSuffix())?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE+classroomSession.getSuffix());
			}
		});
		registerFormatter(ClassroomSessionDivision.class, new AbstractFormatter<ClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSessionDivision classroomSessionDivision, ContentType contentType) {
				return classroomSessionDivision.getTimeDivisionType().getName()+Constant.CHARACTER_SPACE+(classroomSessionDivision.getOrderNumber());
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
					if(identifiable instanceof SubjectClassroomSession)
						return ((SubjectClassroomSession)identifiable).getClassroomSession();
					
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
		
		BusinessServiceProvider.Identifiable.COLLECTION.add(new AbstractActorBusinessImpl.BusinessServiceProviderIdentifiable<Student,Student.SearchCriteria>(Student.class){
			private static final long serialVersionUID = 1322416788278558869L;
			
			@Override
			protected Student.SearchCriteria createSearchCriteria(Service service,DataReadConfiguration dataReadConfiguration) {
				return new Student.SearchCriteria(dataReadConfiguration.getGlobalFilter());
			}
        });
		
		BusinessServiceProvider.Identifiable.COLLECTION.add(new AbstractActorBusinessImpl.BusinessServiceProviderIdentifiable<Teacher,Teacher.SearchCriteria>(Teacher.class){
			private static final long serialVersionUID = 1322416788278558869L;
			
			@Override
			protected Teacher.SearchCriteria createSearchCriteria(Service service,DataReadConfiguration dataReadConfiguration) {
				return new Teacher.SearchCriteria(dataReadConfiguration.getGlobalFilter());
			}
        });
		
		studentEvaluationResultsRankOptions.setType(RankType.EXAEQUO);
		studentEvaluationResultsRankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
	}
	
	@Override
	protected AbstractReportRepository getReportRepository() {
		return inject(SchoolReportRepository.class);
	}
	
	@Override
	protected void persistData() {
		create(inject(IntangibleProductBusiness.class).instanciateOne(SchoolConstant.INTANGIBLE_PRODUCT_TUITION));
    	create(new SalableProduct(getEnumeration(IntangibleProduct.class, SchoolConstant.INTANGIBLE_PRODUCT_TUITION), null));
    	
    	inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE,"certificat d'inscription",Boolean.TRUE, "report/student/registration_certificate.jrxml");
    	inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_TUITION_CERTIFICATE,"certificat de scolarité",Boolean.TRUE, "report/student/tuition_certificate.jrxml");
    	inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET,"bulletin trimestriel",Boolean.TRUE, "report/student/classroom_session_division_sheet.jrxml");
	}
	
	@Override
	public void enableEnterpriseResourcePlanning() {
		StudentBusinessImpl.Listener.COLLECTION.add(new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning(){
    		private static final long serialVersionUID = 1L;
			@Override
			public void beforeCreate(Student student) {
				super.beforeCreate(student);
				if(StringUtils.isBlank(student.getCode())){
					NumberBusiness.FormatArguments orderNumberFormatArguments = new FormatArguments();
					orderNumberFormatArguments.setWidth(4);
					student.setCode("STUD"+Constant.CHARACTER_SLASH+inject(TimeBusiness.class).findYear(inject(AcademicSessionBusiness.class).findCurrent(null).getBirthDate())
							+inject(PersonBusiness.class).findInitials(student.getPerson())+inject(NumberBusiness.class).format(inject(StudentDao.class).countAll()+1,orderNumberFormatArguments)
							+Constant.CHARACTER_HYPHEN+student.getAdmissionLevelTimeDivision().getLevel().getGroup().getCode()
							);
				}
			}
    	});
    	TeacherBusinessImpl.Listener.COLLECTION.add(new TeacherBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
	}

	@Override
	protected void setConstants() {}
	
	@Override
	protected void fakeTransactions() {}
	
	/**/
	

	
	/**/
	
    public static SchoolBusinessLayer getInstance() {
		return INSTANCE;
	}
		
}
