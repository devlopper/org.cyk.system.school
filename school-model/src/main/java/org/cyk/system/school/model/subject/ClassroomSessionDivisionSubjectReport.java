package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionSubjectReport extends AbstractNodeReport<ClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	private SubjectReport subject = new SubjectReport();
	
	public ClassroomSessionDivisionSubjectReport(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		setSource(classroomSessionDivisionSubject);
	}
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			subject.setSource(((ClassroomSessionDivisionSubject)source).getSubject());
			numberOfStudents = format( ((ClassroomSessionDivisionSubject)source).getResults().getNumberOfStudent() );
			average = format( ((ClassroomSessionDivisionSubject)source).getResults().getAverage() );
			
			results.setSource(((ClassroomSessionDivisionSubject)source).getResults());
		}
	}
	
	@Override
	public Map<String, List<?>> createFieldsRandomValues() {
		Map<String, List<?>> map = super.createFieldsRandomValues();
		if(map==null)
			map = new HashMap<>();
		
		map.put(commonUtils.attributePath(AbstractIdentifiable.FIELD_IDENTIFIER, GlobalIdentifier.FIELD_NAME), Arrays.asList("Mathematics","Grammar","Reading & Comprehensive"
				,"Handwriting","Spelling","Phonics","Creative writing","Moral education","Social studies","Science","French","Art & Craft","Music","ICT(Computer)"
				,"Physical education"));
		return map;
	}
	
}
