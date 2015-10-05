package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReport extends AbstractGeneratable<StudentClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private String name,totalMissedTime,totalMissedTimeJustified,average,rank,appreciation,appreciationComments,
		appreciationCommentedBy,totalAverage,totalCoefficient,totalAverageCoefficiented,footer;
	private List<String> markTotals = new ArrayList<>();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	/*
	private LabelValueCollection headerInfos = new LabelValueCollection();
	private LabelValueCollection paymentInfos = new LabelValueCollection();
	private LabelValueCollection taxInfos = new LabelValueCollection();
	*/
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	private ActorReport student = new ActorReport();
	private ActorReport signer = new ActorReport();

	@Override
	public void generate() {
		academicSession.generate();
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
	}

}
