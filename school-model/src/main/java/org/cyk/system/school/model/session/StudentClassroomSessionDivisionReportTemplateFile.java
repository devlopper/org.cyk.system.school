package org.cyk.system.school.model.session;

import java.io.InputStream;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReportTemplateFile extends AbstractReportTemplateFile<StudentClassroomSessionDivisionReportTemplateFile> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private String name,attendedTime,missedTime,missedTimeJustified,average,averageScale,averagePromotionScale,rank,totalAverage,totalCoefficient,totalAverageCoefficiented,
		comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private List<BigDecimal> tempMarkTotals = new ArrayList<>();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	private Boolean generateHeaderImage=Boolean.TRUE,generateBackgroundImage=Boolean.TRUE;
	private InputStream headerImage,backgroundImage;
	
	private List<String> subjectsTableColumnNames = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	private ActorReport student = new ActorReport();
	private ActorReport signer = new ActorReport();
	private ActorReport commentator = new ActorReport();

	@Override
	public void generate() {
		comments = provider.randomText(4, 6, 15, 20);
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
	
		academicSession.getCompany().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		academicSession.generate();
		
		if(Boolean.TRUE.equals(generateHeaderImage))
			headerImage = inputStream(provider.documentHeader().getBytes());
		
		if(Boolean.TRUE.equals(generateBackgroundImage)) 
			backgroundImage = inputStream(provider.companyLogo().getBytes());
		
		student.getPerson().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
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
		
	}
	
	public StudentClassroomSessionDivisionReportTemplateFile addSubjectsTableColumnNames(String...names){
		subjectsTableColumnNames.addAll(Arrays.asList(names));
		return this;
	}
	
}
