package org.cyk.system.school.business.impl.integration;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.RemoteBusiness;
import org.cyk.system.root.business.impl.IdentifiableExcelSheetReader;
import org.cyk.system.root.business.impl.OneDimensionObjectArrayAdapter;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.utility.common.cdi.AbstractBean;

public class Synchronizer extends AbstractBean implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private Context context;
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes;
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes1 = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes2 = new ArrayList<>();
	
	private Collection<Evaluation> evaluations;
	private Collection<Evaluation> evaluations1 = new ArrayList<>();
	private Collection<Evaluation> evaluations2 = new ArrayList<>();
	private Collection<Evaluation> evaluations1New = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations;
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1 = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations2 = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1New = new ArrayList<>();
	
	private String academicSessionCodePrefix = "04/10/200018/06/2001";
	
	public Synchronizer() {
		try {
			context = new InitialContext();
		} catch (NamingException e) {
			e.printStackTrace();
		}
		
		System.out.println("Loading all evaluations...");
		evaluations = inject(RemoteBusiness.class,context, "school-ui-web-primefaces-app").find(Evaluation.class);
		System.out.println("#Evaluations : "+evaluations.size());
		for(Evaluation evaluation : evaluations){
			if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==1l)
				evaluations1.add(evaluation);
			else if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==2l)
				evaluations2.add(evaluation);
		}
		System.out.println("#Evaluations 1 : "+evaluations1.size());
		System.out.println("#Evaluations 2 : "+evaluations2.size());
	}
	
	public void evaluation(){
		System.out.println("Loading all classroomSessionDivisionSubjectEvaluationTypes...");
		classroomSessionDivisionSubjectEvaluationTypes = inject(RemoteBusiness.class,context, "school-ui-web-primefaces-app").find(ClassroomSessionDivisionSubjectEvaluationType.class);
		System.out.println("#ClassroomSessionDivisionSubjectEvaluationTypes : "+classroomSessionDivisionSubjectEvaluationTypes.size());
		for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : classroomSessionDivisionSubjectEvaluationTypes){
			if(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==1l)
				classroomSessionDivisionSubjectEvaluationTypes1.add(classroomSessionDivisionSubjectEvaluationType);
			else if(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==2l)
				classroomSessionDivisionSubjectEvaluationTypes2.add(classroomSessionDivisionSubjectEvaluationType);
		}
		System.out.println("#ClassroomSessionDivisionSubjectEvaluationTypes1 : "+classroomSessionDivisionSubjectEvaluationTypes1.size());
		System.out.println("#ClassroomSessionDivisionSubjectEvaluationTypes2 : "+classroomSessionDivisionSubjectEvaluationTypes2.size());
		
		File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
		IdentifiableExcelSheetReader<Evaluation> excelSheetReader = new IdentifiableExcelSheetReader<Evaluation>(file,Evaluation.class);
    	
    	OneDimensionObjectArrayAdapter<Evaluation> setter = new OneDimensionObjectArrayAdapter<Evaluation>(Evaluation.class);
		
    	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Evaluation> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Evaluation>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public Evaluation instanciate(Object[] values) {
				Evaluation evaluation = new Evaluation();
				evaluation.setClassroomSessionDivisionSubjectEvaluationType(getClassroomSessionDivisionSubjectEvaluationType(values));
				return evaluation;
			}
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key,Object keyType) {
				return Boolean.TRUE;
			}
			
			@Override
			public Evaluation getInstanceByKey(Object[] values, Object key, Object type) {
				return getEvaluation(values);
			}
		};
		
		excelSheetReader.execute();
		twoDimensionObjectArray.setInput(excelSheetReader.getValues());
		twoDimensionObjectArray.execute();
		
		evaluations1New = twoDimensionObjectArray.getOutput();
		System.out.println("#New evaluations1 : "+evaluations1New.size());
	
		inject(RemoteBusiness.class,context, "school-ui-web-primefaces-app").create(Evaluation.class, evaluations1New);
	}
	
	private Evaluation getEvaluation(Object[] values){
		for(Evaluation evaluation : evaluations1)
			if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					,(String)values[1]) && evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject()
					.getSubject().getCode().equals(values[2]) && evaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(values[3]))
				return evaluation;
		
		return null;
	}
	
	private ClassroomSessionDivisionSubjectEvaluationType getClassroomSessionDivisionSubjectEvaluationType(Object[] values){
		for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : classroomSessionDivisionSubjectEvaluationTypes1)
			if(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					,(String)values[1]) && classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject()
					.getSubject().getCode().equals(values[2]) && classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getCode().equals(values[3]))
				return classroomSessionDivisionSubjectEvaluationType;
		return null;
	}
	
	private Boolean isSameSuffix(ClassroomSession classroomSession,String suffix){
		if(classroomSession.getSuffix()==null)
			if(StringUtils.isBlank(suffix))
				return Boolean.TRUE;
			else
				return Boolean.FALSE;
		else
			if(StringUtils.isBlank(suffix))
				return Boolean.FALSE;
			else
				return classroomSession.getSuffix().getCode().equals(suffix);
	}
	
	public void studentClassroomSessionDivisionSubjectEvaluation(){
		File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
		
		System.out.println("Loading all studentClassroomSessionDivisionSubjectEvaluations...");
		studentClassroomSessionDivisionSubjectEvaluations = inject(RemoteBusiness.class,context, "school-ui-web-primefaces-app").find(StudentClassroomSessionDivisionSubjectEvaluation.class);
		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations : "+evaluations.size());
		for(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation : studentClassroomSessionDivisionSubjectEvaluations){
			if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==1l)
				studentClassroomSessionDivisionSubjectEvaluations1.add(studentClassroomSessionDivisionSubjectEvaluation);
			else if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==2l)
				studentClassroomSessionDivisionSubjectEvaluations2.add(studentClassroomSessionDivisionSubjectEvaluation);
		}
		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations 1 : "+evaluations1.size());
		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations 2 : "+evaluations2.size());
		
		IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation>(file,StudentClassroomSessionDivisionSubjectEvaluation.class);
    	
    	OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation> setter = new OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation>(StudentClassroomSessionDivisionSubjectEvaluation.class);
		
    	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public StudentClassroomSessionDivisionSubjectEvaluation instanciate(Object[] values) {
				StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation = new StudentClassroomSessionDivisionSubjectEvaluation();
				studentClassroomSessionDivisionSubjectEvaluation.setEvaluation(getEvaluation(values));
				studentClassroomSessionDivisionSubjectEvaluation.setValue(commonUtils.getBigDecimal((String)values[5]));
				return studentClassroomSessionDivisionSubjectEvaluation;
			}
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key,Object keyType) {
				return Boolean.TRUE;
			}
			
			@Override
			public StudentClassroomSessionDivisionSubjectEvaluation getInstanceByKey(Object[] values, Object key, Object type) {
				return getStudentClassroomSessionDivisionSubjectEvaluation(values);
			}
		};
		
		excelSheetReader.execute();
		twoDimensionObjectArray.setInput(excelSheetReader.getValues());
		twoDimensionObjectArray.execute();
		
		studentClassroomSessionDivisionSubjectEvaluations1New = twoDimensionObjectArray.getOutput();
		System.out.println("#New studentClassroomSessionDivisionSubjectEvaluations : "+studentClassroomSessionDivisionSubjectEvaluations1New.size());
	
		inject(RemoteBusiness.class,context, "school-ui-web-primefaces-app").create(StudentClassroomSessionDivisionSubjectEvaluation.class, studentClassroomSessionDivisionSubjectEvaluations1New);
	}
	
	private StudentClassroomSessionDivisionSubjectEvaluation getStudentClassroomSessionDivisionSubjectEvaluation(Object[] values){
		for(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation : studentClassroomSessionDivisionSubjectEvaluations1)
			if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					,(String)values[1]) && studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject()
					.getSubject().getCode().equals(values[2]) && studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(values[3]) 
					&& studentClassroomSessionDivisionSubjectEvaluation.getStudentClassroomSessionDivisionSubject().getStudent().getCode().equals(values[4]))
				return studentClassroomSessionDivisionSubjectEvaluation;
		
		return null;
	}
	
	public void execute(){
		System.out.println("Starts");
		try {
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Ends");
	}
	
	public static void main(String[] args) {
		Synchronizer synchronizer = new Synchronizer();
		//synchronizer.evaluation();
		synchronizer.studentClassroomSessionDivisionSubjectEvaluation();
	}
}