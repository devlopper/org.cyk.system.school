package org.cyk.system.school.model.session;

import java.io.Serializable;

import org.cyk.system.root.model.file.report.LabelValueCollectionReport;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReport extends AbstractStudentClassroomSessionDivisionReport implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private LabelValueCollectionReport behaviorLabelValueCollection1,behaviorLabelValueCollection2;
	
	@Override
	public void generate() {
		super.generate();
		behaviorLabelValueCollection1 = new LabelValueCollectionReport();
		behaviorLabelValueCollection1.setName("BEHAVIOUR,STUDY AND WORK HABITS");
		for(int i=1;i<=6;i++)
			behaviorLabelValueCollection1.add("B"+i);
		
		behaviorLabelValueCollection2 = new LabelValueCollectionReport();
		behaviorLabelValueCollection2.setName("BEHAVIOUR,STUDY AND WORK HABITS");
		for(int i=1;i<=6;i++)
			behaviorLabelValueCollection2.add("B"+i);
	}
	
}
