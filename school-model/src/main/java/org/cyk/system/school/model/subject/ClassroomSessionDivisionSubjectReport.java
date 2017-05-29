package org.cyk.system.school.model.subject;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.party.person.ActorReport;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionSubjectReport extends AbstractNodeReport<ClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	private SubjectReport subject = new SubjectReport();
	private ActorReport teacher = new ActorReport();
	
	public ClassroomSessionDivisionSubjectReport(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		setSource(classroomSessionDivisionSubject);
	}
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			subject.setSource(((ClassroomSessionDivisionSubject)source).getSubject());
			//numberOfStudents = format( ((ClassroomSessionDivisionSubject)source).getResults().getNumberOfStudent() );
			//average = format( ((ClassroomSessionDivisionSubject)source).getResults().getAverage() );
			teacher.setSource(((ClassroomSessionDivisionSubject)source).getTeacher());
			results.setSource(((ClassroomSessionDivisionSubject)source).getResults());
		}
	}
	
	@Override
	public void generate() {
		super.generate();
		subject.generate();
		teacher.generate();
	}
	
}
