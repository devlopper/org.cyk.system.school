package org.cyk.system.school.ui.web.primefaces.iesa;

import java.io.Serializable;
import java.util.Locale;

import javax.servlet.ServletContextEvent;

import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.structure.EmployeeBusinessImpl;
import org.cyk.system.company.business.impl.structure.EmployeeDetails;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.company.ui.web.primefaces.sale.SaleConsultPage;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.geography.ContactCollectionDetails;
import org.cyk.system.root.business.impl.party.person.JobDetails;
import org.cyk.system.root.business.impl.party.person.MedicalDetails;
import org.cyk.system.root.business.impl.party.person.MedicalInformationsAllergyDetails;
import org.cyk.system.root.business.impl.party.person.MedicalInformationsMedicationDetails;
import org.cyk.system.root.business.impl.party.person.PersonDetails;
import org.cyk.system.root.business.impl.party.person.PersonRelationshipDetails;
import org.cyk.system.root.business.impl.party.person.SignatureDetails;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.MetricCollectionIdentifiableGlobalIdentifier;
import org.cyk.system.root.model.mathematics.MetricValueIdentifiableGlobalIdentifier;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherDetails;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.AbstractSchoolContextListener;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionDivisionConsultPage;
import org.cyk.ui.api.AbstractWindow;
import org.cyk.ui.web.primefaces.AbstractSystemMenuBuilder;
import org.cyk.ui.web.primefaces.page.AbstractPrimefacesPage.PageInstanceManager;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;
import org.cyk.utility.common.generator.AbstractGeneratable;

@javax.servlet.annotation.WebListener
public class ContextListener extends AbstractSchoolContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();
		MetricCollectionIdentifiableGlobalIdentifier.define(ClassroomSessionDivision.class);
		MetricValueIdentifiableGlobalIdentifier.define(StudentClassroomSessionDivision.class);
		
		RootConstant.Configuration.ReportTemplate.LOCALE = Locale.ENGLISH;
		AbstractGeneratable.Listener.Adapter.Default.LOCALE = Locale.ENGLISH;
		
		StudentClassroomSessionDivisionConsultPage.SUBJECT_DETAILS_CLASS_NAME = StudentClassroomSessionDivisionSubjectDetails.class.getName();
		StudentClassroomSessionDivisionConsultPage.LOAD_EVALUATIONS = Boolean.TRUE;
		
		SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT = Boolean.TRUE;
    	
    	SaleConsultPage.SHOW_SALE_PRODUCT_TABLE = Boolean.FALSE;
    	
    	SchoolWebManager.getInstance().getListeners().add(new PrimefacesManager());
    	
    	StudentBusinessImpl.Listener.COLLECTION.add(new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning(){
    		private static final long serialVersionUID = 1L;
    		@Override
    		public String getCodePrefix() {
    			return "IESA";
    		}
    	});
    	
    	TeacherBusinessImpl.Listener.COLLECTION.add(new TeacherBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
    	EmployeeBusinessImpl.Listener.COLLECTION.add(new EmployeeBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
    	
    	AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}
		,AcademicSession.class,Level.class,LevelTimeDivision.class,ClassroomSession.class,ClassroomSessionDivision.class,ClassroomSessionDivisionSubject.class
		,ClassroomSessionDivisionSubjectEvaluationType.class,StudentClassroomSession.class,StudentClassroomSessionDivision.class
		,StudentClassroomSessionDivisionSubject.class);
    	
    	//CompanyConstant.Configuration.SaleCashRegisterMovementCollection.AUTOMATICALLY_GENERATE_REPORT_FILE = Boolean.TRUE;
    	//CompanyConstant.Configuration.Sale.AUTOMATICALLY_GENERATE_REPORT_FILE = Boolean.TRUE;
		
    	CompanyBusinessLayer.getInstance().enableEnterpriseResourcePlanning();
		
    	AbstractCompanyReportProducer.Listener.COLLECTION.add(new AbstractCompanyReportProducer.Listener.Adapter.Default(){
			private static final long serialVersionUID = 215473098986115952L;
			
			@Override
			public String[] getCustomerPersonRelationshipTypeCodes(AbstractIdentifiable identifiable) {
				return new String[]{/*RootConstant.Code.PersonRelationshipType.FAMILY_FATHER,RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER*/};
			}
			
			@Override
			public String getCustomerLabel(AbstractIdentifiable identifiable) {
				return "Parent";
			}
		});
    	
		AbstractWindow.WindowInstanceManager.INSTANCE = new PageInstanceManager(){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean isShowDetails(Class<?> detailsClass,AbstractIdentifiable identifiable,AbstractWindow<?, ?, ?, ?, ?, ?> window) {
				if(identifiable instanceof Person){
					return isClassIn(detailsClass,PersonDetails.class,ContactCollectionDetails.class,JobDetails.class);
				}
				if(identifiable instanceof Employee){
					return isClassIn(detailsClass,EmployeeDetails.class,ContactCollectionDetails.class,SignatureDetails.class);
				}
				if(identifiable instanceof Student){
					return isClassIn(detailsClass,StudentDetails.class,ContactCollectionDetails.class,MedicalDetails.class,MedicalInformationsMedicationDetails.class
							,MedicalInformationsAllergyDetails.class,PersonRelationshipDetails.class);
				}
				if(identifiable instanceof Teacher){
					return isClassIn(detailsClass,TeacherDetails.class,ContactCollectionDetails.class,SignatureDetails.class);
				}
				return super.isShowDetails(detailsClass, identifiable,window);
			}
		};
		
		AbstractSystemMenuBuilder.DEFAULT = SystemMenuBuilder.getInstance();
		
	}
		
	/**/
	
	public static class StudentClassroomSessionDivisionSubjectDetails extends AbstractStudentClassroomSessionDivisionSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT) private String test1;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST1) private String test2;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST2) private String exam;
		public StudentClassroomSessionDivisionSubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
			super(studentSubject);
			for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : studentSubject.getDetails()){
				if(studentSubjectEvaluation.getStudentClassroomSessionDivisionSubject().equals(studentSubject)){
					if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(SchoolConstant.Code.EvaluationType.TEST1))
						test1 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(SchoolConstant.Code.EvaluationType.TEST2))
						test2 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(SchoolConstant.Code.EvaluationType.EXAM))
						exam = numberBusiness.format(studentSubjectEvaluation.getValue());
				}					
			}
		}
		
		public static final String FIELD_TEST1 = "test1";
		public static final String FIELD_TEST2 = "test2";
		public static final String FIELD_EXAM = "exam";
		
	}
	
	/**/

}
