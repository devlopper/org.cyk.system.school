package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Singleton;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.product.IntangibleProductBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.product.IntangibleProduct;
import org.cyk.system.company.model.sale.SalableProduct;
import org.cyk.system.root.business.api.ClazzBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFormatter;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.business.impl.BusinessServiceProvider;
import org.cyk.system.root.business.impl.BusinessServiceProvider.Service;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.root.model.ContentType;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;
import org.cyk.utility.common.computation.DataReadConfiguration;

import lombok.Getter;
import lombok.Setter;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER) @Getter
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Setter private AverageComputationListener averageComputationListener;
	@Setter private Script averageComputationScript;
	/*
	private String actionPrintStudentClassroomSessionTuitionCertificate = "print.tuition.certificate";
	private String actionPrintStudentClassroomSessionRegistrationCertificate = "print.registration.certificate";
	*/
	private String actionCreateSubjectEvaluation = "acse";
	private String actionUpdateStudentClassroomSessionDivisionResults = "auscsdr";
	private String actionComputeStudentClassroomSessionDivisionEvaluationResults = "acscsder";
	private String actionComputeStudentClassroomSessionEvaluationResults = "acscser";
	private String actionComputeStudentClassroomSessionDivisionRankResults = "acscsdrr";
	private String actionComputeStudentClassroomSessionDivisionAttendanceResults = "acscsdar";
	private String actionUpdateStudentClassroomSessionDivisionReportFiles = "auscsdrf";
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
		
		AbstractTypedBusinessService.Listener.MAP.put(Student.class, new StudentBusinessServiceAdapter());
		
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
	
	/*@Override
	protected RootReportProducer getReportProducer() {
		return inject(SchoolReportProducer.class);
	}*/
	
	@Override
	protected void persistData() {
		create(inject(IntangibleProductBusiness.class).instanciateOne(SchoolConstant.INTANGIBLE_PRODUCT_TUITION));
    	create(new SalableProduct(getEnumeration(IntangibleProduct.class, SchoolConstant.INTANGIBLE_PRODUCT_TUITION), null));
    	
    	createReportTemplate(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE,"certificat d'inscription",Boolean.TRUE, "report/student/registration_certificate.jrxml", null, null, null);
    	createReportTemplate(SchoolConstant.REPORT_STUDENT_TUITION_CERTIFICATE,"certificat de scolarité",Boolean.TRUE, "report/student/tuition_certificate.jrxml", null, null, null);
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
