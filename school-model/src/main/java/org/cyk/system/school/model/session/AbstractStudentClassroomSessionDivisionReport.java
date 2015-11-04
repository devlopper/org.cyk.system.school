package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

@Getter @Setter @NoArgsConstructor
public abstract class AbstractStudentClassroomSessionDivisionReport extends AbstractReportTemplateFile<AbstractStudentClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private String name,attendedTime,missedTime,missedTimeJustified,average,averageScale,rank,totalAverage,totalCoefficient,totalAverageCoefficiented,footer,
		comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	private List<String> subjectsTableColumnNames = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	//private List<LabelValueCollectionReport> labelValueCollections = new ArrayList<>();
	private LabelValueCollectionReport studentLabelValueCollection,attendanceLabelValueCollection,overallResultlLabelValueCollection
		,behaviorLabelValueCollection,gradingScaleLabelValueCollection,effortLevelLabelValueCollection,informationLabelValueCollection;
		
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	private ActorReport student = new ActorReport();
	private ActorReport signer = new ActorReport();
	private ActorReport commentator = new ActorReport();

	@Override
	public void generate() {
		comments = provider.randomWord(10, 30);
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
	
		academicSession.getCompany().setGenerateImage(Boolean.TRUE);
		academicSession.generate();
		//TODO to be moved
		academicSession.getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
				+ " OF <style forecolor=\"red\">A</style>BIDJAN");
		
		student.getPerson().setGenerateImage(Boolean.TRUE);
		student.generate();
		signer.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		signer.generate();
		
		commentator.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		commentator.generate();
		
		classroomSessionDivision.generate();
		
		name = "THIRD TERM PRIMARY REPORT CARD";
		attendedTime = positiveFloatNumber(999, 0, 99);
		missedTime = positiveFloatNumber(999, 0, 99);
		missedTimeJustified = positiveFloatNumber(999, 0, 99); 
		totalAverage = positiveFloatNumber(999, 0, 99);
		totalCoefficient = positiveFloatNumber(999, 0, 99);
		totalAverageCoefficiented = positiveFloatNumber(999, 0, 99);
		
		if(classroomSessionDivisionSubjects==null){
			classroomSessionDivisionSubjects = new ArrayList<>();
			for(int i=0;i<15;i++){
				ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject = new ClassroomSessionDivisionSubjectReport();
				classroomSessionDivisionSubject.generate();
				classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
			}
		}
		
		for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			StudentClassroomSessionDivisionSubjectReport subject = new StudentClassroomSessionDivisionSubjectReport(this,classroomSessionDivisionSubject);
			subject.generate();
			subjects.add(subject);
		}
		
		for(int i=0;i<3;i++)
			markTotals.add(positiveFloatNumber(20, 0, 99));
		
		studentLabelValueCollection = new LabelValueCollectionReport();
		studentLabelValueCollection.setName("PUPIL'S DETAILS");
		studentLabelValueCollection.add("Formname(s)", student.getPerson().getNames());
		studentLabelValueCollection.add("Surname", student.getPerson().getSurname());
		studentLabelValueCollection.add("Date of birth", student.getPerson().getBirthDate());
		studentLabelValueCollection.add("Place of birth", student.getPerson().getBirthLocation());
		studentLabelValueCollection.add("Admission No", student.getRegistrationCode());
		studentLabelValueCollection.add("Class", classroomSessionDivision.getClassroomSession().getName());
		studentLabelValueCollection.add("Gender", student.getPerson().getSex());
		
		attendanceLabelValueCollection = new LabelValueCollectionReport();
		attendanceLabelValueCollection.setName("SCHOOL ATTENDANCE");
		attendanceLabelValueCollection.add("Number of times school opened","999");
		attendanceLabelValueCollection.add("Number of times present","999");
		attendanceLabelValueCollection.add("Number of times absent","999");
		
		overallResultlLabelValueCollection = new LabelValueCollectionReport();
		overallResultlLabelValueCollection.setName("OVERALL RESULT");
		overallResultlLabelValueCollection.add("AVERAGE","78.15");
		overallResultlLabelValueCollection.add("GRADE","A+");
		overallResultlLabelValueCollection.add("RANK","24");
		
		behaviorLabelValueCollection = new LabelValueCollectionReport();
		behaviorLabelValueCollection.setName("BEHAVIOUR,STUDY AND WORK HABITS");
		behaviorLabelValueCollection.add("Respects authority", "4");
		behaviorLabelValueCollection.add("Works independently and neatly", "2");
		behaviorLabelValueCollection.add("Completes homework and class work on time", "3");
		behaviorLabelValueCollection.add("Shows social courtesies", "4");
		behaviorLabelValueCollection.add("Demonstrates self-control", "3");
		behaviorLabelValueCollection.add("Takes care of school and others materials", "2");
		behaviorLabelValueCollection.add("Game/Sport", "4");
		behaviorLabelValueCollection.add("Handwriting", "3");
		behaviorLabelValueCollection.add("Drawing/Painting", "4");
		behaviorLabelValueCollection.add("Punctuality/Regularity", "4");
		behaviorLabelValueCollection.add("Works cooperatively in groups", "2");
		behaviorLabelValueCollection.add("Listens and follows directions", "2");
		
		gradingScaleLabelValueCollection = new LabelValueCollectionReport();
		gradingScaleLabelValueCollection.setName("GRADING SCALE");
		LabelValueReport labelValueReport = gradingScaleLabelValueCollection.add("A+", "Excellent");
		labelValueReport.addExtendedValues("90 - 100");
		
		labelValueReport = gradingScaleLabelValueCollection.add("A",  "Very Good");
		labelValueReport.addExtendedValues("80 - 89.99");
		
		labelValueReport = gradingScaleLabelValueCollection.add("B+", "Good");
		labelValueReport.addExtendedValues("70 - 79.99");
		
		labelValueReport = gradingScaleLabelValueCollection.add("B",  "Fair");
		labelValueReport.addExtendedValues("60 - 69.99");
		
		labelValueReport = gradingScaleLabelValueCollection.add("C+", "Satisfactory");
		labelValueReport.addExtendedValues("55 - 59.99");
		
		labelValueReport = gradingScaleLabelValueCollection.add("C",  "Barely satisfactory");
		labelValueReport.addExtendedValues("50 - 54.99");
		
		labelValueReport = gradingScaleLabelValueCollection.add("E",  "Fail");
		labelValueReport.addExtendedValues("00 - 49.99");
		
		effortLevelLabelValueCollection = new LabelValueCollectionReport();
		effortLevelLabelValueCollection.setName("EFFORT LEVELS");
		effortLevelLabelValueCollection.add("1", "Has no regard for the observable traits");
		effortLevelLabelValueCollection.add("2", "Shows minimal regard for the observable traits");
		effortLevelLabelValueCollection.add("3", "Acceptable level of observable traits");
		effortLevelLabelValueCollection.add("4", "Maintains high level of observable traits");
		effortLevelLabelValueCollection.add("5", "Maintains an excellent degree of observable traits");
		
		informationLabelValueCollection = new LabelValueCollectionReport();
		informationLabelValueCollection.setName("HOME/SCHOOL COMMUNICATIONS");
		informationLabelValueCollection.add("ANNUAL AVERAGE","90");
		informationLabelValueCollection.add("ANNUAL GRADE","B+");
		informationLabelValueCollection.add("ANNUAL RANK","25");
		informationLabelValueCollection.add("PROMOTION INFORMATION","PROMOTED");
		informationLabelValueCollection.add("NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015");
		
		subjectsTableColumnNames.add("No.");
		subjectsTableColumnNames.add("SUBJECTS");
		subjectsTableColumnNames.add("Test 1 15%");
		subjectsTableColumnNames.add("Test 2 15%");
		subjectsTableColumnNames.add("Exam 70%");
		subjectsTableColumnNames.add("TOTAL");
		subjectsTableColumnNames.add("GRADE");
		subjectsTableColumnNames.add("RANK");
		subjectsTableColumnNames.add("OUT OF");
		subjectsTableColumnNames.add("MAX");
		subjectsTableColumnNames.add("CLASS AVERAGE");
		subjectsTableColumnNames.add("REMARKS");
		subjectsTableColumnNames.add("TEACHER");
	}
	/*
	public LabelValueCollectionReport getLabelValueCollectionByIndex(Integer index){
		return labelValueCollections.get(index);
	}*/

}
