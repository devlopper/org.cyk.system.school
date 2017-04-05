package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.api.FormatterBusiness;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.file.ScriptBusiness;
import org.cyk.system.root.business.api.geography.ContactCollectionBusiness;
import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.api.value.ValueBusiness.Derive;
import org.cyk.system.root.business.impl.NumberStringFormatter;
import org.cyk.system.root.model.AbstractCollectionItem;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionIdentifiableGlobalIdentifier;
import org.cyk.system.root.persistence.api.file.FileDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionIdentifiableGlobalIdentifierDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionTypeDao;
import org.cyk.system.root.persistence.api.value.ValuePropertiesDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.SchoolConstant.Code.LevelName;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.StudentReportTemplateFile;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.formatter.NumberFormatter;
import org.cyk.utility.common.generator.AbstractGeneratable;

public abstract class AbstractSchoolReportProducer extends AbstractCompanyReportProducer implements SchoolReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public Class<?> getReportTemplateFileClass(AbstractIdentifiable identifiable, String reportTemplateCode) {
		if(identifiable instanceof Student){
			if(SchoolConstant.Code.ReportTemplate.STUDENT_REGISTRATION_CERTIFICATE.equals(reportTemplateCode))
				return StudentReportTemplateFile.class;
			if(SchoolConstant.Code.ReportTemplate.STUDENT_TUITION_CERTIFICATE.equals(reportTemplateCode))
				return StudentReportTemplateFile.class;
		}
		
		if(identifiable instanceof ClassroomSessionDivision){
			return ClassroomSessionDivisionReportTemplateFile.class;
		}
		
		if(identifiable instanceof StudentClassroomSessionDivision){
			return StudentClassroomSessionDivisionReportTemplateFile.class;
		}
		
		return super.getReportTemplateFileClass(identifiable, reportTemplateCode);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <REPORT extends AbstractReportTemplateFile<REPORT>> REPORT produce(Class<REPORT> reportClass, CreateReportFileArguments<?> createReportFileArguments) {
		if(createReportFileArguments.getLocale()==null)
			createReportFileArguments.setLocale(RootConstant.Configuration.ReportTemplate.LOCALE);
		if(StudentReportTemplateFile.class.equals(reportClass)){
			if(createReportFileArguments.getIdentifiable() instanceof Student)
				return (REPORT) produceStudentReport((Student)createReportFileArguments.getIdentifiable());
		}
		
		if(StudentClassroomSessionDivisionReportTemplateFile.class.equals(reportClass)){
			if(createReportFileArguments.getIdentifiable() instanceof StudentClassroomSessionDivision){
				return (REPORT) produceStudentClassroomSessionDivisionReport((StudentClassroomSessionDivision)createReportFileArguments.getIdentifiable()
						,(CreateReportFileArguments<StudentClassroomSessionDivision>) createReportFileArguments);
			}
		}
		
		if(ClassroomSessionDivisionReportTemplateFile.class.equals(reportClass)){
			if(createReportFileArguments.getIdentifiable() instanceof ClassroomSessionDivision){
				return (REPORT) produceClassroomSessionDivisionReport((ClassroomSessionDivision)createReportFileArguments.getIdentifiable()
						,(CreateReportFileArguments<ClassroomSessionDivision>) createReportFileArguments);
			}
		}
		
		return super.produce(reportClass, createReportFileArguments);
	}
	
	private StudentReportTemplateFile produceStudentReport(Student student) {
		StudentReportTemplateFile report = new StudentReportTemplateFile();
		set(student, report.getActor());
		return report;
	}
	
	@Override
	public String getEvaluationTypeCode(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation) {
		return studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode();
	}
	
	@Override
	public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision
			,CreateReportFileArguments<StudentClassroomSessionDivision> createReportFileArguments) {
		StudentClassroomSessionDivisionReportTemplateFile r = createReportTemplateFile(StudentClassroomSessionDivisionReportTemplateFile.class,createReportFileArguments);
		r.getStudentClassroomSessionDivision().setSource(studentClassroomSessionDivision);//TODO all following code should be go into setSource
	
		set(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getGroup()
				.getNodeInformations().getStudentClassroomSessionDivisionResultsReportSigner(), r.getSigner());

		Student student = studentClassroomSessionDivision.getStudent();
		StudentClassroomSessionDivision s = studentClassroomSessionDivision;
		ClassroomSessionDivision csd = s.getClassroomSessionDivision();
		ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
		AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
		NodeResults results = csd.getResults();
		
		//r.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession().setSource(cs);
		r.getStudentClassroomSessionDivision().getAcademicSession().setFromDateToDate(inject(TimeBusiness.class).formatPeriodFromTo(as.getExistencePeriod()));
		
		//TODO Not required anymore because in super 
		ReportTemplate reportTemplate = inject(ClassroomSessionBusiness.class).findCommonNodeInformations(cs).getStudentClassroomSessionDivisionResultsReportTemplate();
		if(reportTemplate.getHeaderImage()!=null)
			r.setHeaderImage(inject(FileBusiness.class).findInputStream(reportTemplate.getHeaderImage()));
		File backgroundImageFile = createReportFileArguments.getBackgroundImageFile();
		if(backgroundImageFile!=null)
			r.setBackgroundImage(inject(FileBusiness.class).findInputStream(backgroundImageFile));
		
		inject(ContactCollectionBusiness.class).load(as.getSchool().getOwnedCompany().getCompany().getContactCollection());
		set(as.getSchool().getOwnedCompany().getCompany().getContactCollection(), r.getStudentClassroomSessionDivision().getAcademicSession().getCompany().getContactCollection());
		set(cs.getCoordinator(), r.getStudentClassroomSessionDivision().getCommentator());
			
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession().getGlobalIdentifier().setName(formatUsingBusiness(cs));
		
		//r.getClassroomSessionDivision().setName(formatUsingBusiness(csd));
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().setAverage(format(results.getAverage()));
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().setHighestAverage(format(results.getAverageHighest()));
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().setLowestAverage(format(results.getAverageLowest()));
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().setNumberOfStudents(inject(NumberBusiness.class).format(results.getNumberOfStudent()));
		r.getStudentClassroomSessionDivision().getClassroomSessionDivision().setOpenedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),csd.getExistencePeriod().getNumberOfMillisecond().get())));
		
		set(student,r.getStudentClassroomSessionDivision().getStudent());
		
		//if(cs.getCoordinator()!=null)
		//	set(cs.getCoordinator(), r.getStudentClassroomSessionDivision().getCommentator());
		
		/*
		if(as.getSchool().getOwnedCompany().getCompany().getSigner()!=null)
			set(as.getSchool().getOwnedCompany().getCompany().getSigner(), r.getSigner());
		*/
		r.getStudentClassroomSessionDivision().setComments(s.getResults().getAppreciation());
		r.getStudentClassroomSessionDivision().setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		/*if(Boolean.TRUE.equals(csd.getStudentEvaluationRequired()) && s.getResults().getEvaluationSort().getAverage().getValue()!=null){
			r.getStudentClassroomSessionDivision().setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
			//TODO a function to be used in jasper report
			//r.getStudentClassroomSessionDivision().setAverageScale(RootConstant.Code.getRelativeCode(s.getResults().getEvaluationSort().getAverageAppreciatedInterval()));
			r.getStudentClassroomSessionDivision().getAverageScale().setSource(s.getResults().getEvaluationSort().getAverageAppreciatedInterval());
			r.getStudentClassroomSessionDivision().setRank(inject(MathematicsBusiness.class).format(s.getResults().getEvaluationSort().getRank()));
			
			r.getStudentClassroomSessionDivision().setAveragePromotionScale(RootConstant.Code.getRelativeCode(s.getResults().getEvaluationSort().getAveragePromotedInterval()));
			
			r.getStudentClassroomSessionDivision().setTotalCoefficient(format(s.getResults().getEvaluationSort().getAverage().getDivisor()));
			r.getStudentClassroomSessionDivision().setTotalAverage(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
			r.getStudentClassroomSessionDivision().setTotalAverageCoefficiented(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
			
		}*/
		
		//r.setName(inject(LanguageBusiness.class).findText("school.report.studentclassroomsessiondivision.results.title",new Object[]{csd.getUiString()}));
		r.getStudentClassroomSessionDivision().setSubjectsBlockTitle(inject(LanguageBusiness.class).findText("school.report.studentclassroomsessiondivision.results.subject"));
		r.getStudentClassroomSessionDivision().setCommentsBlockTitle(inject(LanguageBusiness.class).findText("school.report.studentclassroomsessiondivision.results.comments"));
		r.getStudentClassroomSessionDivision().setSchoolStampBlockTitle(inject(LanguageBusiness.class).findText("school.report.studentclassroomsessiondivision.results.schoolstamp"));
		
		if(s.getResults().getEvaluationSort().getRank()==null)
			;
		else
			processStudentSubjects(r, s,createReportFileArguments);
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r,createReportFileArguments);
		
		return r;
	}
	
	protected void processStudentSubjects(StudentClassroomSessionDivisionReportTemplateFile r,StudentClassroomSessionDivision s,CreateReportFileArguments<?> arguments){
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		Collection<EvaluationType> evaluationTypes = inject(EvaluationTypeDao.class).readAll();
		for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
			Boolean applicable = studentSubject.getResults().getEvaluationSort().getAverage().getValue()!=null;
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubjectReport.setAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverage()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.getGlobalIdentifier().setWeight(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getWeight()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setHighestAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverageHighest()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.getGlobalIdentifier().setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			classroomSessionDivisionSubjectReport.setNumberOfStudents(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getNumberOfStudent()):NOT_APPLICABLE);
			//classroomSessionDivisionSubjectReport.getAverageScale().setSource(studentSubject.getResults().getEvaluationSort().getAverageAppreciatedInterval());
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport(r.getStudentClassroomSessionDivision(),classroomSessionDivisionSubjectReport,null);
			
			r.getStudentClassroomSessionDivision().getSubjects().add(sr);
			
			for(int i=0;i<evaluationTypes.size();i++){
				sr.getMarks().add(NOT_APPLICABLE);
				if(Boolean.TRUE.equals(SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT)){
					r.getStudentClassroomSessionDivision().getTempMarkTotals().add(BigDecimal.ZERO);
					r.getStudentClassroomSessionDivision().getMarkTotals().add(NOT_APPLICABLE);
				}
			}
			
			set(studentSubject.getClassroomSessionDivisionSubject().getTeacher(),sr.getTeacher());
			
			sr.setAverage(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()):NOT_APPLICABLE);
			sr.setAverageCoefficiented(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()
					.multiply(studentSubject.getClassroomSessionDivisionSubject().getWeight()==null?BigDecimal.ONE:studentSubject.getClassroomSessionDivisionSubject().getWeight())):NOT_APPLICABLE);
			
			NumberFormatter.String rankFormatter = new NumberFormatter.String.Adapter.Default(studentSubject.getResults().getEvaluationSort().getRank().getValue(),null);
			rankFormatter.setIsOrdinal(Boolean.TRUE);
			rankFormatter.setLocale(AbstractGeneratable.Listener.Adapter.Default.LOCALE);
			rankFormatter.setIsAppendOrdinalSuffix(Boolean.TRUE);
			rankFormatter.setIsAppendExaequo(Boolean.TRUE.equals(studentSubject.getResults().getEvaluationSort().getRank().getExaequo()));
			sr.setRank(applicable?rankFormatter.execute():NOT_APPLICABLE);	
			
			//if(studentSubject.getResults().getEvaluationSort().getAverageInterval()!=null){
				set(studentSubject.getResults().getEvaluationSort().getAverageAppreciatedInterval(), sr.getAverageScale());
				if(applicable){
					sr.getAverageScale().getGlobalIdentifier().setCode(RootConstant.Code.getRelativeCode(studentSubject.getResults().getEvaluationSort().getAverageAppreciatedInterval()));
				}
			//}else
				//sr.getAverageScale().setCode(NOT_APPLICABLE);
			
			BigDecimal[] results = new BigDecimal[]{BigDecimal.ZERO};
			studentSubjectEvaluation(sr, studentSubject, /*studentSubject.getDetails()*/studentSubjectEvaluations, results,evaluationTypes,arguments);
		}
		
		if(Boolean.TRUE.equals(SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT)){
			for(int i=0;i<evaluationTypes.size();i++)
				if(i < r.getStudentClassroomSessionDivision().getMarkTotals().size())
					r.getStudentClassroomSessionDivision().getMarkTotals().set(i,format(r.getStudentClassroomSessionDivision().getTempMarkTotals().get(i)));
		}
		
		
	}
	
	protected void studentSubjectEvaluation(StudentClassroomSessionDivisionSubjectReport sr,StudentClassroomSessionDivisionSubject studentSubject,Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations,BigDecimal[] results
			,Collection<EvaluationType> evaluationTypes,CreateReportFileArguments<?> arguments){
		int i = 0;
		for(EvaluationType evaluationType : evaluationTypes){
			for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluations){
				if(getEvaluationTypeCode(studentSubjectEvaluation).equals(evaluationType.getCode()) 
						&& studentSubjectEvaluation.getStudentClassroomSessionDivisionSubject().getIdentifier().equals(studentSubject.getIdentifier()) ){
					BigDecimal value = getMarkValue(studentSubjectEvaluation);
					if(Boolean.TRUE.equals(SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT))
						sr.getStudentClassroomSessionDivision().getTempMarkTotals().set(i, sr.getStudentClassroomSessionDivision().getTempMarkTotals().get(i).add(value));
					
					sr.getMarks().set(i,format(value));
				}
			}
			i++;
		}
	}
	
	protected BigDecimal getMarkValue(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation){
		BigDecimal value = studentSubjectEvaluation.getValue();
		if(Boolean.FALSE.equals(studentSubjectEvaluation.getEvaluation().getCoefficientApplied()))
			value = value.multiply(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getWeight()==null
			?BigDecimal.ONE:studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getWeight());
		return value;
	}
	
	protected void produceStudentClassroomSessionDivisionReportLabelValueCollections(StudentClassroomSessionDivisionReportTemplateFile r,CreateReportFileArguments<?> arguments){
		
	}
	
	public ClassroomSessionDivisionReportTemplateFile produceClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision
			,CreateReportFileArguments<ClassroomSessionDivision> createReportFileArguments) {
		createReportFileArguments.setIdentifiableName(classroomSessionDivision.getName()+" Broadsheet ");
		classroomSessionDivision.getClassroomSessionDivisionSubjects().addMany(inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivision(classroomSessionDivision));
		classroomSessionDivision.getStudentClassroomSessionDivisions().addMany(inject(StudentClassroomSessionDivisionDao.class).readByClassroomSessionDivision(classroomSessionDivision));
		Collection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class)
				.readByClassroomSessionDivision(classroomSessionDivision);
		for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : studentClassroomSessionDivisionSubjects){
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : classroomSessionDivision.getStudentClassroomSessionDivisions().getCollection()){
				if(studentClassroomSessionDivisionSubject.getStudent().equals(studentClassroomSessionDivision.getStudent())){
					studentClassroomSessionDivision.getStudentClassroomSessionDivisionSubjects().addOne(studentClassroomSessionDivisionSubject);
					break;
				}
			}
		}
		ClassroomSessionDivisionReportTemplateFile r = createReportTemplateFile(ClassroomSessionDivisionReportTemplateFile.class,createReportFileArguments);
		//r.setHeaderImage(inject(FileBusiness.class).findInputStream(inject(FileDao.class).read(SchoolConstant.Code.File.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS)));
		r.getClassroomSessionDivision().setSource(classroomSessionDivision);
		r.setName( classroomSessionDivision.getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getCode()
				+( classroomSessionDivision.getClassroomSession().getSuffix() == null ? Constant.EMPTY_STRING 
						: classroomSessionDivision.getClassroomSession().getSuffix().getCode())+" Broadsheet");
		r.getClassroomSessionDivision().getLabelValueCollection().getCollection().clear();
		
		Integer numberOfSubjects = classroomSessionDivision.getClassroomSessionDivisionSubjects().getCollection().size();
		LabelValueReport labelValueAverageScore = r.getClassroomSessionDivision().getLabelValueCollection().add("Average Score").setExtendedValuesSize(numberOfSubjects);
		LabelValueReport labelValueNumberOfStudentsEvaluated = r.getClassroomSessionDivision().getLabelValueCollection().add("Number of students evaluated").setExtendedValuesSize(numberOfSubjects);
		LabelValueReport labelValuePassFraction = r.getClassroomSessionDivision().getLabelValueCollection().add("Pass Fraction").setExtendedValuesSize(numberOfSubjects);
		LabelValueReport labelValuePassPercentage = r.getClassroomSessionDivision().getLabelValueCollection().add("Pass Percentage").setExtendedValuesSize(numberOfSubjects);
		LabelValueReport labelValueFailFraction = r.getClassroomSessionDivision().getLabelValueCollection().add("Fail Fraction").setExtendedValuesSize(numberOfSubjects);
		LabelValueReport labelValueFailPercentage = r.getClassroomSessionDivision().getLabelValueCollection().add("Fail Percentage").setExtendedValuesSize(numberOfSubjects);
		
		
		NumberStringFormatter numberStringFormatter = new NumberStringFormatter(null, null);
		numberStringFormatter.setIsPercentage(Boolean.TRUE);
		int i = 0;
		for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject : r.getClassroomSessionDivision().getClassroomSessionDivisionSubjects()){
			labelValueAverageScore.getExtendedValues()[i] = classroomSessionDivisionSubject.getAverage();
			labelValueNumberOfStudentsEvaluated.getExtendedValues()[i] = classroomSessionDivisionSubject.getResults().getNumberOfStudent();
			
			BigDecimal passFraction = new BigDecimal(((ClassroomSessionDivisionSubject)classroomSessionDivisionSubject.getSource()).getResults().getNumberOfStudentPassingEvaluationAverage()
					).divide(new BigDecimal(((ClassroomSessionDivisionSubject)classroomSessionDivisionSubject.getSource()).getResults().getNumberOfStudent()), 4
							, RoundingMode.HALF_DOWN);
			
			labelValuePassFraction.getExtendedValues()[i] = classroomSessionDivisionSubject.getResults().getNumberOfStudentPassingEvaluationAverage()
					+"/"+classroomSessionDivisionSubject.getResults().getNumberOfStudent();
			labelValuePassPercentage.getExtendedValues()[i] = numberStringFormatter.setInput(passFraction).execute();
			
			labelValueFailFraction.getExtendedValues()[i] = (classroomSessionDivisionSubject.getResults().getNumberOfStudentNotPassingEvaluationAverage())
					+"/"+classroomSessionDivisionSubject.getResults().getNumberOfStudent();
			labelValueFailPercentage.getExtendedValues()[i] = numberStringFormatter.setInput(BigDecimal.ONE.subtract(passFraction)).execute();
			i++;
		}
			
		return r;
	}

	/**/
	
	public static interface Listener extends AbstractCompanyReportProducer.Listener {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		public static class Adapter extends AbstractCompanyReportProducer.Listener.Adapter implements Listener,Serializable {
			private static final long serialVersionUID = 1L;
			
			/**/
			
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = 1L;
				
			}
			
		}
	}
	
	/**/
	
	public static class Default extends AbstractSchoolReportProducer implements Serializable{
		private static final long serialVersionUID = 1L;
		
		@Override
		public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				CreateReportFileArguments<StudentClassroomSessionDivision> arguments) {
			LabelValueCollectionReport labelValueCollectionReport;
			StudentClassroomSessionDivisionReportTemplateFile report = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,arguments);
			
			String levelNameCode = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getCode();
			String effortLevelsMetricCollectionCode = null,effortLevelsIntervalCollectionCode = null,schoolCommunicationMetricCollectionCode=null;
			addPupilsDetails(report);
			addAttednanceDetails(report, studentClassroomSessionDivision, StringUtils.startsWith(levelNameCode, "G") ? 
					SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT : SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT);
			
			if(arguments.getReportTemplate().getNameScript()!=null){
				arguments.getReportTemplate().getNameScript().getInputs().clear();
				arguments.getReportTemplate().getNameScript().getInputs().put("studentClassroomSessionDivision", studentClassroomSessionDivision);
				arguments.getReportTemplate().getNameScript().getInputs().put(RootConstant.Configuration.Script.FORMATTER_BUSINESS, inject(FormatterBusiness.class));
				arguments.getReportTemplate().getNameScript().getInputs().put(RootConstant.Configuration.Script.IS_DRAFT,report.getIsDraft());
				
				arguments.getReportTemplate().getNameScript().getInputs().put(RootConstant.Configuration.ScriptVariable.LOCALE,arguments.getLocale());
				report.setName((String) inject(ScriptBusiness.class).evaluate(arguments.getReportTemplate().getNameScript()));
			}
			//arguments.setIdentifiableName(report.getName());
			
			if(arguments.getReportTemplate().getResultFileNamingScript()!=null){
				arguments.getReportTemplate().getResultFileNamingScript().getInputs().clear();
				arguments.getReportTemplate().getResultFileNamingScript().getInputs().put("studentClassroomSessionDivision", studentClassroomSessionDivision);
				arguments.getReportTemplate().getResultFileNamingScript().getInputs().put(RootConstant.Configuration.Script.FORMATTER_BUSINESS, inject(FormatterBusiness.class));
				arguments.getReportTemplate().getResultFileNamingScript().getInputs().put(RootConstant.Configuration.Script.IS_DRAFT,report.getIsDraft());
				arguments.getReportTemplate().getResultFileNamingScript().getInputs().put(RootConstant.Configuration.ScriptVariable.LOCALE,arguments.getLocale());
				arguments.setIdentifiableName((String) inject(ScriptBusiness.class).evaluate(arguments.getReportTemplate().getResultFileNamingScript()));
				//arguments.getFile().setName((String) inject(ScriptBusiness.class).evaluate(arguments.getReportTemplate().getResultFileNamingScript()));
			}
			
			if(arguments.getReportTemplate().getFooterScript()!=null){
				arguments.getReportTemplate().getFooterScript().getInputs().clear();
				arguments.getReportTemplate().getFooterScript().getInputs().put("studentClassroomSessionDivision", studentClassroomSessionDivision);
				arguments.getReportTemplate().getFooterScript().getInputs().put(RootConstant.Configuration.Script.IS_DRAFT,report.getIsDraft());
				report.setFooter((String) inject(ScriptBusiness.class).evaluate(arguments.getReportTemplate().getFooterScript()));
			}
			/*
			Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers = inject(MetricCollectionIdentifiableGlobalIdentifierDao.class)
				.readByCriteria(new MetricCollectionIdentifiableGlobalIdentifier.SearchCriteria().addIdentifiableGlobalIdentifier(studentClassroomSessionDivision
				.getClassroomSessionDivision()).addMetricCollectionType(inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_KINDERGARTEN_STUDENT)));
					MetricCollection metricCollection = metricCollectionIdentifiableGlobalIdentifiers.iterator().next().getMetricCollection();
			*/
			if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.PK,SchoolConstant.Code.LevelName.K1,SchoolConstant.Code.LevelName.K2
					,SchoolConstant.Code.LevelName.K3},levelNameCode)){
				
				Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers = inject(MetricCollectionIdentifiableGlobalIdentifierDao.class)
				.readByCriteria(new MetricCollectionIdentifiableGlobalIdentifier.SearchCriteria().addIdentifiableGlobalIdentifier(studentClassroomSessionDivision.getClassroomSessionDivision())
						.addMetricCollectionType(inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_KINDERGARTEN_STUDENT)));
				
				MetricCollection metricCollection = metricCollectionIdentifiableGlobalIdentifiers.iterator().next().getMetricCollection();
				effortLevelsMetricCollectionCode = metricCollection.getCode();
				effortLevelsIntervalCollectionCode = metricCollection.getValueProperties().getIntervalCollection().getCode();
				schoolCommunicationMetricCollectionCode = SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT;
				
				addMetricCollectionsByType(report, studentClassroomSessionDivision, metricCollectionIdentifiableGlobalIdentifiers);
				
			}else{
				Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes = inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class)
						.readByClassroomSessionDivision(studentClassroomSessionDivision.getClassroomSessionDivision());
				String testCoef = null,examCoef = null;
				
				NumberStringFormatter numberStringFormatter = (NumberStringFormatter) new NumberStringFormatter(null, null).setIsPercentage(Boolean.TRUE)
						.setLocale(inject(LanguageBusiness.class).findCurrentLocale());
				for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : classroomSessionDivisionSubjectEvaluationTypes){
					if(testCoef!=null && examCoef!=null)
						break;
					numberStringFormatter.setInput(classroomSessionDivisionSubjectEvaluationType.getWeight());
					if(testCoef==null && classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getCode().equals(SchoolConstant.Code.EvaluationType.TEST1))
						testCoef = inject(NumberBusiness.class).format(numberStringFormatter);
					else if(examCoef==null && classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getCode().equals(SchoolConstant.Code.EvaluationType.EXAM))
						examCoef = inject(NumberBusiness.class).format(numberStringFormatter);
				}
				if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G1,SchoolConstant.Code.LevelName.G2,SchoolConstant.Code.LevelName.G3},levelNameCode)){
					effortLevelsMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_PRIMARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G4,SchoolConstant.Code.LevelName.G5,SchoolConstant.Code.LevelName.G6},levelNameCode)){
					effortLevelsMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_PRIMARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G7,SchoolConstant.Code.LevelName.G8,SchoolConstant.Code.LevelName.G9},levelNameCode)){
					effortLevelsMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_SECONDARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G10,SchoolConstant.Code.LevelName.G11,SchoolConstant.Code.LevelName.G12},levelNameCode)){
					effortLevelsMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_SECONDARY_STUDENT;
				}
				schoolCommunicationMetricCollectionCode = SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT;
				report.getStudentClassroomSessionDivision().addSubjectsTableColumnNames("No.","SUBJECTS","TEST 1 "+testCoef,"TEST 2 "+testCoef,"EXAM  "+examCoef,"TOTAL 100%","GRADE","RANK"
						,"OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
				
				addOverallResult(report);
				//System.out.println("Effort level : "+effortLevelsMetricCollectionCode);
				addMetricCollection(report, ((StudentClassroomSessionDivision)report.getSource()), effortLevelsMetricCollectionCode);
				report.getCurrentLabelValueCollection().setName(StringUtils.upperCase(report.getCurrentLabelValueCollection().getName()));
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName(report.getCurrentLabelValueCollection().getName());
				labelValueCollectionReport.setCollection(report.getCurrentLabelValueCollection().getCollection().subList(7, 14));
				report.getCurrentLabelValueCollection().setCollection(report.getCurrentLabelValueCollection().getCollection().subList(0, 7));
				
				report.addLabelValueCollection(labelValueCollectionReport);
				
				addIntervalCollection(report,inject(ClassroomSessionBusiness.class).findCommonNodeInformations(
					((StudentClassroomSessionDivision)report.getSource()).getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionDivisionAverageScale()
					,null,Boolean.FALSE,Boolean.TRUE,new Integer[][]{{1,2}});
				report.getCurrentLabelValueCollection().setName(StringUtils.upperCase(report.getCurrentLabelValueCollection().getName()));		
			}
			
			addIntervalCollection(report,inject(IntervalCollectionDao.class).read(effortLevelsIntervalCollectionCode)
					,inject(MetricCollectionDao.class).read(effortLevelsMetricCollectionCode).getValueProperties(),Boolean.TRUE,Boolean.FALSE,null);
			if(LevelName.K1.equals(levelNameCode)){
				addIntervalCollection(report,inject(IntervalCollectionDao.class).read(SchoolConstant.Code.IntervalCollection.METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT)
						,inject(ValuePropertiesDao.class).read(SchoolConstant.Code.ValueProperties.METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT),Boolean.TRUE,Boolean.FALSE,null);
			}
			addSchoolCommunications(report, studentClassroomSessionDivision,schoolCommunicationMetricCollectionCode);
			
			CommonNodeInformations commonNodeInformations = inject(ClassroomSessionBusiness.class).findCommonNodeInformations(
					((StudentClassroomSessionDivision)report.getStudentClassroomSessionDivision().getSource()).getClassroomSessionDivision().getClassroomSession());
			
			Collection<StudentClassroomSessionDivisionReport> previousStudentClassroomSessionDivisionReports = new ArrayList<>();
			for(int orderNumber = commonNodeInformations.getClassroomSessionDivisionOrderNumberInterval().getLow().getValue().intValue() ; 
					orderNumber < studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber() ; orderNumber++){
				StudentClassroomSessionDivisionReport previousStudentClassroomSessionDivisionReport = new StudentClassroomSessionDivisionReport();
				previousStudentClassroomSessionDivisionReport.setSource(inject(StudentClassroomSessionDivisionDao.class)
						.readByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent()
								, inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(studentClassroomSessionDivision
										.getClassroomSessionDivision().getClassroomSession(), new Long(orderNumber))));
				report.getStudentClassroomSessionDivision().setPrevious(previousStudentClassroomSessionDivisionReport);
				previousStudentClassroomSessionDivisionReports.add(previousStudentClassroomSessionDivisionReport);
			}
			
			if(levelNameCode.startsWith("G"))//TODO do it better
				addPreviousResult(report,previousStudentClassroomSessionDivisionReports);
			
			return report;
		}
		
		protected void addPupilsDetails(StudentClassroomSessionDivisionReportTemplateFile report){
			addValueCollection(report, SchoolConstant.Code.ValueCollection.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_STUDENT
					,new Derive.Adapter.Default().addInputs(report,report.getStudentClassroomSessionDivision().getStudent()
							,report.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession()));
		}
		
		protected void addAttednanceDetails(StudentClassroomSessionDivisionReportTemplateFile report,StudentClassroomSessionDivision studentClassroomSessionDivision
				,String metricCollectionCode){
			addValueCollection(report, SchoolConstant.Code.ValueCollection.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_ATTENDANCE
					,new Derive.Adapter.Default().addInputs(report,studentClassroomSessionDivision.getClassroomSessionDivision(),
							inject(ClassroomSessionBusiness.class).findCommonNodeInformations(studentClassroomSessionDivision.getClassroomSessionDivision()
									.getClassroomSession()).getAttendanceTimeDivisionType()));
			addMetricCollection(report, studentClassroomSessionDivision, metricCollectionCode,new Derive.Adapter.Default().addInputs(report
					,studentClassroomSessionDivision.getClassroomSessionDivision()),Boolean.FALSE);
			
		}
		
		protected void addOverallResult(StudentClassroomSessionDivisionReportTemplateFile report){
			report.addLabelValues("OVERALL RESULT", new String[][]{
				{"AVERAGE",report.getStudentClassroomSessionDivision().getAverage()}
				,{"GRADE",RootConstant.Code.getRelativeCode((AbstractCollectionItem<?>) report.getStudentClassroomSessionDivision().getAverageScale().getSource())}
				,{"RANK",report.getStudentClassroomSessionDivision().getRank()}
			});
		}
		
		protected void addPreviousResult(StudentClassroomSessionDivisionReportTemplateFile report,Collection<StudentClassroomSessionDivisionReport> studentClassroomSessionDivisions){
			if(studentClassroomSessionDivisions==null || studentClassroomSessionDivisions.isEmpty())
				return;
			List<String> averages = new ArrayList<>();
			averages.add("AVERAGE");
			for(StudentClassroomSessionDivisionReport studentClassroomSessionDivision : studentClassroomSessionDivisions)
				averages.add(studentClassroomSessionDivision.getAverage());
			
			List<String> grades = new ArrayList<>();
			grades.add("GRADE");
			for(StudentClassroomSessionDivisionReport studentClassroomSessionDivision : studentClassroomSessionDivisions)
				grades.add(studentClassroomSessionDivision.getAverageScale().getSource() ==null ? "NULL" : 
					RootConstant.Code.getRelativeCode((AbstractCollectionItem<?>) studentClassroomSessionDivision.getAverageScale().getSource()));
			
			List<String> ranks = new ArrayList<>();
			ranks.add("RANK");
			for(StudentClassroomSessionDivisionReport studentClassroomSessionDivision : studentClassroomSessionDivisions)
				ranks.add(studentClassroomSessionDivision.getRank());
			
			List<String> trimesters = new ArrayList<>();
			trimesters.add("TRIMESTER");
			for(StudentClassroomSessionDivisionReport studentClassroomSessionDivision : studentClassroomSessionDivisions)
				trimesters.add(((StudentClassroomSessionDivision)studentClassroomSessionDivision.getSource()).getClassroomSessionDivision().getOrderNumber().toString());
			
			report.addLabelValues("PREVIOUS TERMS", new String[][]{
				averages.toArray(new String[]{})
				,grades.toArray(new String[]{})
				,ranks.toArray(new String[]{})
				,trimesters.toArray(new String[]{})
			});
			
			
		}
		
		protected void addSchoolCommunications(StudentClassroomSessionDivisionReportTemplateFile report,StudentClassroomSessionDivision studentClassroomSessionDivision,String metricCollectionCode){
			addMetricCollection(report, studentClassroomSessionDivision,metricCollectionCode);
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber()==inject(ClassroomSessionBusiness.class)
					.findCommonNodeInformations(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getClassroomSessionDivisionOrderNumberInterval().getHigh().getValue().intValue()){
				StudentResults classroomSessionResults = inject(StudentClassroomSessionDao.class)
						.readByStudentByClassroomSession(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getResults();
				
				report.addLabelValue("ANNUAL AVERAGE",format(classroomSessionResults.getEvaluationSort().getAverage().getValue()));
				report.addLabelValue("ANNUAL GRADE"
						,classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval()==null?NULL_VALUE:RootConstant.Code
								.getRelativeCode(classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval()));
				report.addLabelValue("ANNUAL RANK",inject(MathematicsBusiness.class).format(classroomSessionResults.getEvaluationSort().getRank()));
				report.addLabelValue("PROMOTION INFORMATION",
						classroomSessionResults.getEvaluationSort().getAveragePromotedInterval()==null?NULL_VALUE:classroomSessionResults.getEvaluationSort()
								.getAveragePromotedInterval().getName().toUpperCase());
				report.addLabelValue("NEXT ACADEMIC SESSION",format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
						.getAcademicSession().getNextStartingDate()));
				
			}else{
				ClassroomSessionDivision nextClassroomSessionDivision = inject(ClassroomSessionDivisionDao.class)
						.readByClassroomSessionByOrderNumber(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
								,(studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber()+1));
				if(nextClassroomSessionDivision==null){
					
				}else{
					
					
				}
				
			}
			report.getStudentClassroomSessionDivision().getClassroomSessionDivision().setSource(studentClassroomSessionDivision.getClassroomSessionDivision());
			addValueCollection(report, SchoolConstant.Code.ValueCollection.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_SCHOOL_COMMUNICATION
					, new Derive.Adapter.Default().addInputs(report,report.getStudentClassroomSessionDivision().getClassroomSessionDivision()),Boolean.FALSE);
		}
		
	}
	
	/**/
	
	public static final String LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID = "school.report.studentclassroomsessiondivision.results.block.overallresult.grade";
	
}
