package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.mathematics.IntervalReport;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionSubjectReport extends AbstractGeneratable<StudentClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private StudentClassroomSessionDivisionReport studentClassroomSessionDivision;
	private ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject;
	
	private String average,averageCoefficiented,rank;
	private IntervalReport averageScale = new IntervalReport();
	private List<String> marks = new ArrayList<>();
	private ActorReport teacher = new ActorReport();
	
	public StudentClassroomSessionDivisionSubjectReport(StudentClassroomSessionDivisionReport studentClassroomSessionDivision,ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject) {
		super();
		this.studentClassroomSessionDivision = studentClassroomSessionDivision;
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
	}
	
	@Override
	public void generate() {
		average = positiveFloatNumber(999, 0, 99);
		averageCoefficiented = positiveFloatNumber(999, 0, 99);
		rank = positiveFloatNumber(999, 0, 99);
		for(int i=0;i<3;i++)
			marks.add(positiveFloatNumber(999, 0, 99));
		teacher.generate();
		averageScale.generate();
	}
	
	@Override
	public String toString() {
		return classroomSessionDivisionSubject.getName()+" | "+marks;
	}
}