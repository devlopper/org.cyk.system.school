package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.model.subject.AbstractNodeReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

@Getter @Setter
public class ClassroomSessionDivisionReport extends AbstractNodeReport<ClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private ClassroomSessionReport classroomSession = new ClassroomSessionReport();
	private ArrayList<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects = new ArrayList<>();
	private ArrayList<StudentClassroomSessionDivisionReport> studentClassroomSessionDivisions = new ArrayList<>();
	
	public ClassroomSessionDivisionReport() {
		
	}
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		classroomSession.setSource( ((ClassroomSessionDivision)source).getClassroomSession() );
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : ((ClassroomSessionDivision)source).getClassroomSessionDivisionSubjects().getCollection()){
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport(classroomSessionDivisionSubject);
			classroomSessionDivisionSubjects.add(classroomSessionDivisionSubjectReport);
		}
		
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : ((ClassroomSessionDivision)source).getStudentClassroomSessionDivisions().getCollection()){
			studentClassroomSessionDivision.getStudentClassroomSessionDivisionSubjects().setSynchonizationEnabled(Boolean.TRUE);
			StudentClassroomSessionDivisionReport studentClassroomSessionDivisionReport = new StudentClassroomSessionDivisionReport(this,studentClassroomSessionDivision);
			studentClassroomSessionDivisions.add(studentClassroomSessionDivisionReport);
			studentClassroomSessionDivisionReport.setClassroomSessionDivisionSubjects(classroomSessionDivisionSubjects);
		}
		
		results.setSource(((ClassroomSessionDivision)source).getResults());
	}
	
	public ClassroomSessionDivisionSubjectReport getClassroomSessionDivisionSubjectAtIndex(Integer index){
		return index == null || index >= classroomSessionDivisionSubjects.size() ? null : classroomSessionDivisionSubjects.get(index);
	}
	
	@Override
	public void generate() {
		super.generate();
		labelValueCollection.getCollection().clear();
		labelValueCollection.add("Average Score",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.add("Number of student evaluated",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.add("Pass Fraction",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.add("Pass Percentage",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.add("Fail Fraction",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.add("Fail Percentage",String.valueOf(provider.randomInt(0, 100)));
		labelValueCollection.generateExtendedValues(20);
		classroomSession.generate();
	}
	
	public void generateSubjects(Integer numberOfSubjects){
		for(int i = 0 ; i < numberOfSubjects ; i++){
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubject.generate();
			classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
		}
	}
	
	public void generateStudent(Integer numberOfStudents,Boolean skipable){
		for(int i = 0 ; i < numberOfStudents ; i++){
			StudentClassroomSessionDivisionReport studentClassroomSessionDivision = new StudentClassroomSessionDivisionReport(this);
			studentClassroomSessionDivision.generate();
			studentClassroomSessionDivision.generateSubjects(classroomSessionDivisionSubjects, skipable);
			studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
		}
	}
	
	@Override
	public Map<String, List<?>> createFieldsRandomValues() {
		Map<String, List<?>> map = super.createFieldsRandomValues();
		if(map==null)
			map = new HashMap<>();
		
		List<String> names = new ArrayList<>();
		for(String order : new String[]{"First","Second","Third"})
			for(String level : new String[]{"Primary","Secondary"})
				names.add(order+" Term "+level+" Report Card");
		
		map.put(commonUtils.attributePath(AbstractIdentifiable.FIELD_IDENTIFIER, GlobalIdentifier.FIELD_NAME), names);
		return map;
	}
		
	/**/
	
}
