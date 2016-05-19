package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@SuppressWarnings("unchecked")
public class StudentClassroomSessionDivisionDetails extends AbstractOutputDetails<StudentClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationCode,names,numberOfTimeAbsent,globalAppreciation,conferenceRequested;
	
	@Input @InputText private String subject0Average,subject1Average,subject2Average,subject3Average,subject4Average,subject5Average,subject6Average,subject7Average
		,subject8Average;
	@Input @InputText private String subject9Average,subject10Average,subject11Average,subject12Average,subject13Average,subject14Average,subject15Average,subject16Average;
	@Input @InputText private String subject17Average,subject18Average,subject19Average,subject20Average,subject21Average,subject22Average,subject23Average,subject24Average;
	@Input @InputText private String subject25Average,subject26Average,subject27Average,subject28Average,subject29Average,subject30Average,subject31Average,subject32Average;
	
	@Input @InputText private String evaluationAverageDividend,evaluationAverageDivisor,evaluationAverageValue,evaluationRankValue;
	
	public StudentClassroomSessionDivisionDetails(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super(studentClassroomSessionDivision);
		registrationCode = studentClassroomSessionDivision.getStudent().getRegistration().getCode();
		names = studentClassroomSessionDivision.getStudent().getPerson().getNames();
		globalAppreciation = studentClassroomSessionDivision.getResults().getAppreciation();
		conferenceRequested = formatResponse(studentClassroomSessionDivision.getResults().getConferenceRequested());
		
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDividend()!=null)
			evaluationAverageDividend = numberBusiness.format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDividend());
		
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDivisor()!=null)
			evaluationAverageDivisor = numberBusiness.format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDivisor());
		
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()!=null)
			evaluationAverageValue = numberBusiness.format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue());
		
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getRank().getValue()!=null)
			evaluationRankValue = RootBusinessLayer.getInstance().getMathematicsBusiness().format(studentClassroomSessionDivision.getResults().getEvaluationSort().getRank());
		
		if(studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()!=null)
			numberOfTimeAbsent = numberBusiness.format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().convertAttendanceTimeToDivisionDuration(
					studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession(),studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()));
	}
	
	/**/
	
	public static final String FIELD_REGISTRATION_CODE = "registrationCode";
	public static final String FIELD_NAMES = "names";
	public static final String FIELD_EVALUATION_AVERAGE_DIVIDEND = "evaluationAverageDividend";
	public static final String FIELD_EVALUATION_AVERAGE_DIVISOR = "evaluationAverageDivisor";
	public static final String FIELD_EVALUATION_AVERAGE_VALUE = "evaluationAverageValue";
	public static final String FIELD_EVALUATION_RANK_VALUE = "evaluationRankValue";
	public static final String FIELD_NUMBER_OF_TIME_ABSENT = "numberOfTimeAbsent";
	public static final String FIELD_GLOBAL_APPRECIATION = "globalAppreciation";
	public static final String FIELD_CONFERENCE_REQUESTED = "conferenceRequested";
	
	private static final String PREFIX_SUBJECT = "subject";
	private static final String SUFFIX_AVERAGE = "Average";
	
	public static Boolean isSubjectAverageFieldName(String name){
		return StringUtils.isNotBlank(StringUtils.substringBetween(name,PREFIX_SUBJECT, SUFFIX_AVERAGE));
	}
	
	private static String getSubjectAverageFieldName(Integer index){
		return PREFIX_SUBJECT+index+SUFFIX_AVERAGE;
	}
	
	public static Integer getSubjectAverageFieldNameIndex(String name){
		return Integer.valueOf(StringUtils.substringBetween(name, PREFIX_SUBJECT, SUFFIX_AVERAGE));
	}
	
	/**/
	
	public static final Set<String> FIELDS_SIMPLE = new HashSet<>();
	public static final Set<String> FIELDS_BROAD_SHEET = new HashSet<>();
	
	static{
		add(new Set[]{FIELDS_SIMPLE}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE,FIELD_NUMBER_OF_TIME_ABSENT,FIELD_GLOBAL_APPRECIATION,FIELD_CONFERENCE_REQUESTED);
		add(new Set[]{FIELDS_BROAD_SHEET}, FIELD_REGISTRATION_CODE,FIELD_NAMES,FIELD_EVALUATION_AVERAGE_DIVIDEND,FIELD_EVALUATION_AVERAGE_DIVISOR,FIELD_EVALUATION_AVERAGE_VALUE,FIELD_EVALUATION_RANK_VALUE);
		for(int i = 0 ; i<31 ; i++)
			add(new Set[]{FIELDS_BROAD_SHEET}, getSubjectAverageFieldName(i));
	}
	
	public static void add(Set<String>[] sets,String...fieldNames){
		for(Set<String> set : sets)
			for(String fieldName : fieldNames)
				set.add(fieldName);
	}
}