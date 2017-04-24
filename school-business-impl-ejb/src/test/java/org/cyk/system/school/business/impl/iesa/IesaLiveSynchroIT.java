package org.cyk.system.school.business.impl.iesa;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.IdentifiableExcelSheetReader;
import org.cyk.system.root.business.impl.OneDimensionObjectArrayAdapter;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

public class IesaLiveSynchroIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes;
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes1 = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes2 = new ArrayList<>();
	
	private Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects;
	private Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects1 = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects2 = new ArrayList<>();
	
	private Collection<Evaluation> evaluations;
	private Collection<Evaluation> evaluations1 = new ArrayList<>();
	private Collection<Evaluation> evaluations2 = new ArrayList<>();
	private Collection<Evaluation> evaluations1New = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations;
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1 = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations2 = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1New = new ArrayList<>();
    
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	try{
    		
    		System.out.println("Loading all evaluations...");
    		evaluations = inject(EvaluationDao.class).readAll();
    		System.out.println("#Evaluations : "+evaluations.size());
    		for(Evaluation evaluation : evaluations){
    			if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==1l)
    				evaluations1.add(evaluation);
    			else if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==2l)
    				evaluations2.add(evaluation);
    		}
    		System.out.println("#Evaluations 1 : "+evaluations1.size());
    		System.out.println("#Evaluations 2 : "+evaluations2.size());
    		
    		/**/
    		
    		File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
    		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
    		
    		System.out.println("Loading all studentClassroomSessionDivisionSubjectEvaluations...");
    		studentClassroomSessionDivisionSubjectEvaluations = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readAll();
    		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations : "+studentClassroomSessionDivisionSubjectEvaluations.size());
    		for(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation : studentClassroomSessionDivisionSubjectEvaluations){
    			if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==1l)
    				studentClassroomSessionDivisionSubjectEvaluations1.add(studentClassroomSessionDivisionSubjectEvaluation);
    			else if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getOrderNumber()==2l)
    				studentClassroomSessionDivisionSubjectEvaluations2.add(studentClassroomSessionDivisionSubjectEvaluation);
    		}
    		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations 1 : "+studentClassroomSessionDivisionSubjectEvaluations1.size());
    		System.out.println("#StudentClassroomSessionDivisionSubjectEvaluations 2 : "+studentClassroomSessionDivisionSubjectEvaluations2.size());
    		
    		System.out.println("Loading all classroomSessionDivisionSubjects...");
    		classroomSessionDivisionSubjects = inject(ClassroomSessionDivisionSubjectDao.class).readAll();
    		System.out.println("#classroomSessionDivisionSubjects : "+classroomSessionDivisionSubjects.size());
    		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
    			if(classroomSessionDivisionSubject.getClassroomSessionDivision().getOrderNumber()==1l)
    				classroomSessionDivisionSubjects1.add(classroomSessionDivisionSubject);
    			else if(classroomSessionDivisionSubject.getClassroomSessionDivision().getOrderNumber()==2l)
    				classroomSessionDivisionSubjects2.add(classroomSessionDivisionSubject);
    		}
    		System.out.println("#classroomSessionDivisionSubjects 1 : "+classroomSessionDivisionSubjects1.size());
    		System.out.println("#classroomSessionDivisionSubjects 2 : "+classroomSessionDivisionSubjects2.size());
    		
    		IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation>(file,StudentClassroomSessionDivisionSubjectEvaluation.class);
        	
        	OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation> setter = new OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation>(StudentClassroomSessionDivisionSubjectEvaluation.class);
    		
        	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation>(setter){
    			private static final long serialVersionUID = 1L;
    			
    			@Override
    			public StudentClassroomSessionDivisionSubjectEvaluation instanciate(Object[] values) {
    				StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation = new StudentClassroomSessionDivisionSubjectEvaluation();
    				studentClassroomSessionDivisionSubjectEvaluation.setEvaluation(getEvaluation(values));
    				studentClassroomSessionDivisionSubjectEvaluation.setValue(commonUtils.getBigDecimal((String)values[5]));
    				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = inject(StudentClassroomSessionDivisionSubjectDao.class)
    						.readByStudentByClassroomSessionDivisionBySubject(inject(StudentDao.class), classroomSessionDivision, subject);
    				studentClassroomSessionDivisionSubjectEvaluation.setStudentClassroomSessionDivisionSubject();
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
    	
    		inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).create(studentClassroomSessionDivisionSubjectEvaluations1New);
    		
    		
    	}catch(Exception exception){
    		exception.printStackTrace();
    	}
    	
    	System.exit(0);
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
	
	private ClassroomSessionDivisionSubject getClassroomSessionDivisionSubject(Object[] values){
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects1)
			if(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
					,(String)values[1]) && classroomSessionDivisionSubject
					.getSubject().getCode().equals(values[2]))
				return classroomSessionDivisionSubject;
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
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer()
    			.setStructurationEnabled(Boolean.FALSE)
    			.setSynchronizationEnabled(Boolean.FALSE)
    			.setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getDivisionOrderNumbers().clear();
    	return dataProducer;
    }
        
}
