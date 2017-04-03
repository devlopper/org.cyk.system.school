package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.system.school.model.subject.AbstractNodeReport;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionReport extends AbstractNodeReport<ClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private ClassroomSessionReport classroomSession = new ClassroomSessionReport();
	private BroadsheetReport broadsheet = new BroadsheetReport();
	private ArrayList<StudentClassroomSessionDivisionReport> studentClassroomSessionDivisions = new ArrayList<>();
	
	@Override
	public void generate() {
		super.generate();
		classroomSession.generate();
		broadsheet.generate();
	}
	
	public void generateStudent(){
		for(int i = 0 ; i < 3 ; i++){
			StudentClassroomSessionDivisionReport studentClassroomSessionDivision = new StudentClassroomSessionDivisionReport(this);
			studentClassroomSessionDivision.generate();
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
