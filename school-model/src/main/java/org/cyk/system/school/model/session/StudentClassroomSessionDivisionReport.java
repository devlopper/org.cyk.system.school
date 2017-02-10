package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReport extends AbstractStudentNodeReport<StudentClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private ActorReport student = new ActorReport();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	private String attendedTime,missedTime,missedTimeJustified,averagePromotionScale,totalAverage,totalCoefficient,totalAverageCoefficiented,
		comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private List<BigDecimal> tempMarkTotals = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	
	private ActorReport commentator = new ActorReport();
	
	private List<String> subjectsTableColumnNames = new ArrayList<>();
	
	public StudentClassroomSessionDivisionReport(ClassroomSessionDivisionReport classroomSessionDivision) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
	}
	
	@Override
	public void generate() {
		super.generate();
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
	
		academicSession.getCompany().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		academicSession.getCompany().setGenerateBackground(Boolean.TRUE);
		academicSession.generate();
		
		student.getPerson().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		student.generate();
		
		commentator.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		commentator.generate();
		
		//name = "THIRD TERM PRIMARY REPORT CARD";
		attendedTime = positiveFloatNumber(999, 0, 99);
		missedTime = positiveFloatNumber(999, 0, 99);
		missedTimeJustified = positiveFloatNumber(999, 0, 99); 
		totalAverage = positiveFloatNumber(999, 0, 99);
		totalCoefficient = positiveFloatNumber(999, 0, 99);
		totalAverageCoefficiented = positiveFloatNumber(999, 0, 99);
		comments = provider.randomText(4, 6, 15, 20);
		
		if(classroomSessionDivisionSubjects==null){
			classroomSessionDivisionSubjects = new ArrayList<>();
			for(int i=0;i<18;i++){
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
	}
	
	public StudentClassroomSessionDivisionReport addSubjectsTableColumnNames(String...names){
		subjectsTableColumnNames.addAll(Arrays.asList(names));
		return this;
	}
	
	@Override
	public String toString() {
		return classroomSessionDivision.getName()+" | "+marks;
	}
}