package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Set;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.root.model.file.File;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.utility.common.FileExtensionGroup;
import org.cyk.utility.common.annotation.user.interfaces.FileExtensions;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputFile;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Getter @Setter public abstract class AbstractStudentResultsOutputDetails<LEVEL,STUDENT_LEVEL extends AbstractStudentResult<LEVEL, DETAIL>,DETAIL> extends AbstractOutputDetails<STUDENT_LEVEL> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@IncludeInputs private StudentDetails student;
	@Input @InputFile(extensions=@FileExtensions(groups=FileExtensionGroup.IMAGE)) protected File studentImage;
	//TODO take it from student
	@Input @InputText private String studentCode,studentName,studentLastnames,studentBirthDate;
	
	//TODO to be deleted , use __fXX__ from super class
	@Input @InputText private String detail0Average,detail1Average,detail2Average,detail3Average,detail4Average,detail5Average,detail6Average,detail7Average
		,detail8Average;
	@Input @InputText private String detail9Average,detail10Average,detail11Average,detail12Average,detail13Average,detail14Average,detail15Average,detail16Average;
	@Input @InputText private String detail17Average,detail18Average,detail19Average,detail20Average,detail21Average,detail22Average,detail23Average,detail24Average;
	@Input @InputText private String detail25Average,detail26Average,detail27Average,detail28Average,detail29Average,detail30Average,detail31Average,detail32Average;
	//TODO do it better
	@Input @InputText private String evaluationAverageDividend,evaluationAverageDivisor,evaluationAverageValue,evaluationRankValue;
	
	public AbstractStudentResultsOutputDetails(STUDENT_LEVEL studentLevel) {
		super(studentLevel);
		if(studentLevel==null)
			return;
		student = new StudentDetails(studentLevel.getStudent());
		studentImage = student.getImage();
		studentCode = student.getCode();
		studentName = student.getName();
		studentLastnames = student.getLastnames();
		studentBirthDate = student.getBirthDate();
		//student = studentLevel.getStudent().getCode()+Constant.CHARACTER_SLASH+studentLevel.getStudent().getPerson().getNames();
		
		if(studentLevel.getResults().getEvaluationSort().getAverage().getDividend()!=null)
			evaluationAverageDividend = numberBusiness.format(studentLevel.getResults().getEvaluationSort().getAverage().getDividend());
		
		if(studentLevel.getResults().getEvaluationSort().getAverage().getDivisor()!=null)
			evaluationAverageDivisor = numberBusiness.format(studentLevel.getResults().getEvaluationSort().getAverage().getDivisor());
		
		if(studentLevel.getResults().getEvaluationSort().getAverage().getValue()!=null)
			evaluationAverageValue = numberBusiness.format(studentLevel.getResults().getEvaluationSort().getAverage().getValue());
		
		if(studentLevel.getResults().getEvaluationSort().getRank().getValue()!=null)
			evaluationRankValue = inject(MathematicsBusiness.class).format(studentLevel.getResults().getEvaluationSort().getRank());
		
	}
	
	/**/
	
	public static final String FIELD_STUDENT = "student";
	public static final String FIELD_STUDENT_IMAGE = "studentImage";
	public static final String FIELD_STUDENT_CODE = "studentCode";
	public static final String FIELD_STUDENT_NAME = "studentName";
	public static final String FIELD_STUDENT_LASTNAMES = "studentLastnames";
	public static final String FIELD_STUDENT_BIRTHDATE = "studentBirthDate";
	//public static final String FIELD_REGISTRATION_CODE = "registrationCode";
	//public static final String FIELD_NAMES = "names";
	public static final String FIELD_EVALUATION_AVERAGE_DIVIDEND = "evaluationAverageDividend";
	public static final String FIELD_EVALUATION_AVERAGE_DIVISOR = "evaluationAverageDivisor";
	public static final String FIELD_EVALUATION_AVERAGE_VALUE = "evaluationAverageValue";
	public static final String FIELD_EVALUATION_RANK_VALUE = "evaluationRankValue";
	
	private static final String PREFIX_DETAIL = "detail";
	private static final String SUFFIX_AVERAGE = "Average";
	
	public static Boolean isDetailAverageFieldName(String name){
		return StringUtils.isNotBlank(StringUtils.substringBetween(name,PREFIX_DETAIL, SUFFIX_AVERAGE));
	}
	
	protected static String getDetailAverageFieldName(Integer index){
		return PREFIX_DETAIL+index+SUFFIX_AVERAGE;
	}
	
	public static Integer getDetailAverageFieldNameIndex(String name){
		return Integer.valueOf(StringUtils.substringBetween(name, PREFIX_DETAIL, SUFFIX_AVERAGE));
	}
	
	/**/
	
	public static void add(Set<String>[] sets,String...fieldNames){
		for(Set<String> set : sets)
			for(String fieldName : fieldNames)
				set.add(fieldName);
	}
	
	public static void configureBroadsheetFieldNames(Set<String> set){
		for(int i = 0 ; i<31 ; i++)
			set.add(getDetailAverageFieldName(i));
	}
}