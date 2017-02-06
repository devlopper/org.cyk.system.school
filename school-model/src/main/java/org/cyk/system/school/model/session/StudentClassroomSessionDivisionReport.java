package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
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

	private ClassroomSessionDivisionReport classroomSessionDivision;
	
	private String attendedTime,missedTime,missedTimeJustified,averagePromotionScale,totalAverage,totalCoefficient,totalAverageCoefficiented,
	comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private List<BigDecimal> tempMarkTotals = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	private ActorReport student = new ActorReport();
	private ActorReport signer = new ActorReport();
	private ActorReport commentator = new ActorReport();
	
	public StudentClassroomSessionDivisionReport(ClassroomSessionDivisionReport classroomSessionDivision) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
	}
	
	@Override
	public String toString() {
		return classroomSessionDivision.getName()+" | "+marks;
	}
}