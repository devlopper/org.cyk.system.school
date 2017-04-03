package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.subject.AbstractNodeReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

@Getter @Setter
public class ClassroomSessionDivisionReport extends AbstractNodeReport<ClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private ClassroomSessionReport classroomSession = new ClassroomSessionReport();
	private ArrayList<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects = new ArrayList<>();
	private ArrayList<StudentClassroomSessionDivisionReport> studentClassroomSessionDivisions = new ArrayList<>();
	
	public ClassroomSessionDivisionReport() {
		labelValueCollection.add("Average Score");
		labelValueCollection.add("Number of student evaluated");
		labelValueCollection.add("Pass Fraction");
		labelValueCollection.add("Pass Percentage");
		labelValueCollection.add("Fail Fraction");
		labelValueCollection.add("Fail Percentage");
	}
	
	public ClassroomSessionDivisionSubjectReport getClassroomSessionDivisionSubjectAtIndex(Integer index){
		return index == null || index >= classroomSessionDivisionSubjects.size() ? null : classroomSessionDivisionSubjects.get(index);
	}
	
	@Override
	public void generate() {
		super.generate();
		classroomSession.generate();
	}
	
	public void generateSubjects(Integer numberOfSubjects){
		for(int i = 0 ; i < numberOfSubjects ; i++){
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubject.generate();
			classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
		}
	}
	
	public void generateStudent(){
		for(int i = 0 ; i < 3 ; i++){
			StudentClassroomSessionDivisionReport studentClassroomSessionDivision = new StudentClassroomSessionDivisionReport(this);
			studentClassroomSessionDivision.generate();
			studentClassroomSessionDivision.generateSubjects(classroomSessionDivisionSubjects, Boolean.FALSE);
			studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
		}
	}
	
	@Override
	protected List<String> getRandomNames() {
		return RANDOM_NAMES;
	}
	
	/**/
	
	public static final List<String> RANDOM_NAMES = new ArrayList<>();
	static{
		for(String order : new String[]{"First","Second","Third"})
			for(String level : new String[]{"Primary","Secondary"})
				RANDOM_NAMES.add(order+" Term "+level+" Report Card");
	}
	
}
