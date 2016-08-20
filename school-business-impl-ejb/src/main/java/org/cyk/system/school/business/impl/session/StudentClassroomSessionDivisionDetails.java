package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsOutputDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @SuppressWarnings("unchecked")
public class StudentClassroomSessionDivisionDetails extends AbstractStudentResultsOutputDetails<ClassroomSessionDivision,StudentClassroomSessionDivision,StudentClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String numberOfTimeAbsent,globalAppreciation,conferenceRequested;
	
	public StudentClassroomSessionDivisionDetails() {
		this(null);
	}
	
	public StudentClassroomSessionDivisionDetails(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super(studentClassroomSessionDivision);
		if(studentClassroomSessionDivision==null)
			return;
		globalAppreciation = studentClassroomSessionDivision.getResults().getAppreciation();
		conferenceRequested = formatResponse(studentClassroomSessionDivision.getResults().getConferenceRequested());
		if(studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()!=null)
			numberOfTimeAbsent = numberBusiness.format(inject(ClassroomSessionBusiness.class).convertAttendanceTimeToDivisionDuration(
					studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession(),studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()));
	}
	
	/**/
	
	public static final String FIELD_NUMBER_OF_TIME_ABSENT = "numberOfTimeAbsent";
	public static final String FIELD_GLOBAL_APPRECIATION = "globalAppreciation";
	public static final String FIELD_CONFERENCE_REQUESTED = "conferenceRequested";
	
	public static final Set<String> FIELDS_SIMPLE = new HashSet<>();
	public static final Set<String> FIELDS_BROAD_SHEET = new HashSet<>();
	
	static{
		add(new Set[]{FIELDS_SIMPLE}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE,FIELD_NUMBER_OF_TIME_ABSENT,FIELD_GLOBAL_APPRECIATION,FIELD_CONFERENCE_REQUESTED);
		add(new Set[]{FIELDS_BROAD_SHEET}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_DIVIDEND,FIELD_EVALUATION_AVERAGE_DIVISOR,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE);
		configureBroadsheetFieldNames(FIELDS_BROAD_SHEET);
	}
	
	
}