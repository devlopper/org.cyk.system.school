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
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

@Getter @Setter @NoArgsConstructor
public abstract class AbstractStudentClassroomSessionDivisionReport extends AbstractReportTemplateFile<AbstractStudentClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private String name,totalMissedTime,totalMissedTimeJustified,average,rank,
		commentedBy,totalAverage,totalCoefficient,totalAverageCoefficiented,footer,classCoordinatorComments,
		comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	private List<String> subjectsTableColumnNames = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	//private List<LabelValueCollectionReport> labelValueCollections = new ArrayList<>();
	private LabelValueCollectionReport pupilLabelValueCollection,attendanceLabelValueCollection,overallResultlLabelValueCollection
		,behaviorLabelValueCollection,gradingScaleLabelValueCollection,effortLevelLabelValueCollection,informationLabelValueCollection;
		
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	private ActorReport student = new ActorReport();
	private ActorReport signer = new ActorReport();

	@Override
	public void generate() {
		comments = provider.randomWord(10, 30);
		subjectsBlockTitle = provider.randomWord(10, 30);
		commentsBlockTitle = provider.randomWord(10, 30);
		schoolStampBlockTitle = provider.randomWord(10, 30);
	
		academicSession.getCompany().setGenerateImage(Boolean.TRUE);
		academicSession.generate();
		academicSession.getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
				+ " OF <style forecolor=\"red\">A</style>BIDJAN");
		
		student.getPerson().setGenerateImage(Boolean.TRUE);
		student.generate();
		signer.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		signer.generate();
		classroomSessionDivision.generate();
		
		name = "Report card";
		totalMissedTime = positiveFloatNumber(999, 0, 99);
		totalMissedTimeJustified = positiveFloatNumber(999, 0, 99); 
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
		
		pupilLabelValueCollection = new LabelValueCollectionReport();
		pupilLabelValueCollection.add("Formname(s)", student.getPerson().getNames());
		pupilLabelValueCollection.add("Surname", student.getPerson().getSurname());
		pupilLabelValueCollection.add("Date of birth", student.getPerson().getBirthDate());
		pupilLabelValueCollection.add("Place of birth", student.getPerson().getBirthLocation());
		pupilLabelValueCollection.add("Admission No", student.getRegistrationCode());
		pupilLabelValueCollection.add("Class", classroomSessionDivision.getClassroomSession().getName());
		pupilLabelValueCollection.add("Gender", student.getPerson().getSex());
		
		attendanceLabelValueCollection = new LabelValueCollectionReport();
		attendanceLabelValueCollection.add("Number of times school opened");
		attendanceLabelValueCollection.add("Number of times present");
		attendanceLabelValueCollection.add("Number of times absent");
		
		overallResultlLabelValueCollection = new LabelValueCollectionReport();
		overallResultlLabelValueCollection.add("Average");
		overallResultlLabelValueCollection.add("Grade");
		overallResultlLabelValueCollection.add("Rank");
		
		behaviorLabelValueCollection = new LabelValueCollectionReport();
		for(int i=1;i<=12;i++)
			behaviorLabelValueCollection.add("B"+i);
		
		gradingScaleLabelValueCollection = new LabelValueCollectionReport();
		gradingScaleLabelValueCollection.add("A+", "");
		gradingScaleLabelValueCollection.add("A", "");
		gradingScaleLabelValueCollection.add("B+", "");
		gradingScaleLabelValueCollection.add("B", "");
		gradingScaleLabelValueCollection.add("C+", "");
		gradingScaleLabelValueCollection.add("C", "");
		gradingScaleLabelValueCollection.add("E", "");
		
		effortLevelLabelValueCollection = new LabelValueCollectionReport();
		for(int i=1;i<=5;i++)
			effortLevelLabelValueCollection.add("E"+i);
		
		informationLabelValueCollection = new LabelValueCollectionReport();
		informationLabelValueCollection.add("Annual average");
		informationLabelValueCollection.add("Annual grade");
		informationLabelValueCollection.add("Annual rank");
		informationLabelValueCollection.add("Promotion information");
		informationLabelValueCollection.add("Next academic year");
		
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
