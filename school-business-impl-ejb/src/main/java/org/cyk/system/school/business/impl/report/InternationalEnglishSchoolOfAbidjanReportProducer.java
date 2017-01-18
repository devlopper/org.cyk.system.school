package org.cyk.system.school.business.impl.report;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.api.file.ScriptBusiness;
import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.value.ValueBusiness.Derive;
import org.cyk.system.root.business.impl.NumberStringFormatter;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionIdentifiableGlobalIdentifier;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionIdentifiableGlobalIdentifierDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionTypeDao;
import org.cyk.system.root.persistence.api.value.ValuePropertiesDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;

public  class InternationalEnglishSchoolOfAbidjanReportProducer extends AbstractSchoolReportProducer.Default implements Serializable{
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
		
		arguments.getReportTemplate().getResultFileNamingScript().getInputs().clear();
		arguments.getReportTemplate().getResultFileNamingScript().getInputs().put("studentClassroomSessionDivision", studentClassroomSessionDivision);
		report.setName((String) inject(ScriptBusiness.class).evaluate(arguments.getReportTemplate().getResultFileNamingScript()));
		arguments.setIdentifiableName(report.getName());
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
			report.addSubjectsTableColumnNames("No.","SUBJECTS","TEST 1 "+testCoef,"TEST 2 "+testCoef,"EXAM  "+examCoef,"TOTAL 100%","GRADE","RANK"
					,"OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
			
			labelValueCollectionReport = new LabelValueCollectionReport();
			labelValueCollectionReport.setName("OVERALL RESULT");
			labelValueCollectionReport.add("AVERAGE",report.getAverage());
			labelValueCollectionReport.add("GRADE",report.getAverageScale());
			if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentRankable()))
				labelValueCollectionReport.add("RANK",report.getRank());
			report.addLabelValueCollection(labelValueCollectionReport);
			
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
		if(SchoolConstant.Code.LevelName.K1.equals(levelNameCode)){
			addIntervalCollection(report,inject(IntervalCollectionDao.class).read(SchoolConstant.Code.IntervalCollection.METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT)
					,inject(ValuePropertiesDao.class).read(SchoolConstant.Code.ValueProperties.METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT),Boolean.TRUE,Boolean.FALSE,null);
		}
		addSchoolCommunications(report, studentClassroomSessionDivision,schoolCommunicationMetricCollectionCode);
		return report;
	}
	
	protected void addPupilsDetails(StudentClassroomSessionDivisionReportTemplateFile report){
		addValueCollection(report, SchoolConstant.Code.ValueCollection.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_STUDENT
				,new Derive.Adapter.Default().addInputs(report,report.getStudent(),report.getClassroomSessionDivision().getClassroomSession()));
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
		report.getClassroomSessionDivision().setSource(studentClassroomSessionDivision.getClassroomSessionDivision());
		addValueCollection(report, SchoolConstant.Code.ValueCollection.STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_SCHOOL_COMMUNICATION
				, new Derive.Adapter.Default().addInputs(report,report.getClassroomSessionDivision()),Boolean.FALSE);
	}

}