package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.AbstractStudentResultsOutputDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@SuppressWarnings("unchecked") @Getter @Setter
public class StudentClassroomSessionDetails extends AbstractStudentResultsOutputDetails<ClassroomSession,StudentClassroomSession,StudentClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@IncludeInputs private ClassroomSessionDetails classroomSessionDetails;
	@Input @InputText private String tuition,classroomSession;
	
	public StudentClassroomSessionDetails() {
		this(null);
	}
	
	public StudentClassroomSessionDetails(StudentClassroomSession studentClassroomSession) {
		super(studentClassroomSession);
		if(studentClassroomSession==null)
			return;
		classroomSessionDetails = new ClassroomSessionDetails(studentClassroomSession.getClassroomSession());
		classroomSession = studentClassroomSession.getClassroomSession().getCode();
		/*if(studentClassroomSession.getTuitionSale()==null){
			
		}else{
			tuition = formatNumber(studentClassroomSession.getTuitionSale().getCost().getValue());
		}*/
		
	}
	
	/**/
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	
	public static final Set<String> FIELDS_SIMPLE = new HashSet<>();
	public static final Set<String> FIELDS_BROAD_SHEET = new HashSet<>();
	
	static{
		add(new Set[]{FIELDS_SIMPLE}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE);
		add(new Set[]{FIELDS_BROAD_SHEET}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_DIVIDEND,FIELD_EVALUATION_AVERAGE_DIVISOR,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE);
		configureBroadsheetFieldNames(FIELDS_BROAD_SHEET);
	}
}