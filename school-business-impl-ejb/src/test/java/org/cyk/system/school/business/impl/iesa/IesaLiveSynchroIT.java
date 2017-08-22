package org.cyk.system.school.business.impl.iesa;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.IdentifiableExcelSheetReader;
import org.cyk.system.root.business.impl.OneDimensionObjectArrayAdapter;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.Constant;

public class IesaLiveSynchroIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
	private Collection<Subject> subjects;
	
	private Collection<Person> persons;
	private Collection<Person> personsUpdated = new ArrayList<>();
	
	private Collection<Student> students;
	private Collection<Student> studentsUpdated = new ArrayList<>();
	
	private Collection<ClassroomSession> classroomSessions;
	private Collection<ClassroomSessionDivision> classroomSessionDivisions1;
	private Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects1;
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes1;
	
	private Collection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects1;
	
	private Collection<Evaluation> evaluations1;
	private Collection<Evaluation> evaluations1New = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1 = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1New = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations1Update = new ArrayList<>();
    
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	try{
    		System.out.println("Loading datas...");
    		subjects = inject(SubjectDao.class).readAll();
    		print(Subject.class,subjects);
    		
    		persons = inject(PersonDao.class).readAll();
    		print(Person.class,persons);
    		
    		students = inject(StudentDao.class).readAll();
    		print(Student.class,students);
    		
    		classroomSessions = inject(ClassroomSessionDao.class).readAll();
    		print(ClassroomSession.class,classroomSessions);
    		
    		classroomSessionDivisions1 = inject(ClassroomSessionDivisionDao.class).readByClassroomSessionsByOrderNumber(classroomSessions, 1l);
    		print(ClassroomSessionDivision.class,classroomSessionDivisions1);
    		
    		classroomSessionDivisionSubjects1 = inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisions(classroomSessionDivisions1);
    		print(ClassroomSessionDivisionSubject.class,classroomSessionDivisionSubjects1);
    		
    		classroomSessionDivisionSubjectEvaluationTypes1 = inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).readByClassroomSessionDivisionSubjects(classroomSessionDivisionSubjects1);
    		print(ClassroomSessionDivisionSubjectEvaluationType.class,classroomSessionDivisionSubjectEvaluationTypes1);
    		
    		studentClassroomSessionDivisionSubjects1 = inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisions(classroomSessionDivisions1);
    		print(StudentClassroomSessionDivisionSubject.class, studentClassroomSessionDivisionSubjects1);
    		
    		studentClassroomSessionDivisionSubjectEvaluations1 = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readByClassroomSessionDivisionSubjects(classroomSessionDivisionSubjects1);
    		print(StudentClassroomSessionDivisionSubjectEvaluation.class, studentClassroomSessionDivisionSubjectEvaluations1);
    		
    		evaluations1 = inject(EvaluationDao.class).readByClassroomSessionDivisionSubjectEvaluationTypes(classroomSessionDivisionSubjectEvaluationTypes1);
    		print(Evaluation.class, evaluations1);
    		/*
    		System.out.println("1###############################################");
    		for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : classroomSessionDivisionSubjectEvaluationTypes1)
    			if(classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getCode().startsWith("TEST") && 
    					classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getSubject().getCode().equals("ART_CRAFT")
    					&& classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision()
    					.getClassroomSession().getLevelTimeDivision().getLevel().getCode().equals("G1"))
    			System.out.println(classroomSessionDivisionSubjectEvaluationType);
    		
    		System.out.println("2###############################################");
    		for(Evaluation evaluation : evaluations1)
    			if(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().startsWith("TEST") && 
    					evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getSubject().getCode().equals("ART_CRAFT")
    					&& evaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision()
    					.getClassroomSession().getLevelTimeDivision().getLevel().getCode().equals("G1"))
    			System.out.println(evaluation);
    		*/
    		/*
    		debug(getClassroomSessionDivisionSubjectEvaluationType(new Object[]{"G1","A","ART_CRAFT","TEST1",""}));
    		debug(getClassroomSessionDivisionSubjectEvaluationType(new Object[]{"G1","A","ART_CRAFT","TEST2",""}));
    		*/
    		
    		//debug(getEvaluation(new Object[]{"G1","A","ART_CRAFT","TEST1",""}));
    		//debug(getEvaluation(new Object[]{"G1","A","ART_CRAFT","TEST2",""}));
    		
    		/**/
    		
    		evaluation();
    		
    		marks();
    		
    		persons();
    	}catch(Exception exception){
    		exception.printStackTrace();
    	}
    	
    	System.exit(0);
    }
    
    private void print(Class<?> aClass,Collection<?> collection){
    	System.out.println("#"+aClass.getSimpleName()+" 1 : "+collection.size());
    }
    
    public void evaluation(){
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
				if(evaluation.getClassroomSessionDivisionSubjectEvaluationType()==null)
					return null;
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
	
		create(evaluations1New);
		evaluations1.addAll(evaluations1New);
	}
    
    public void marks(){
    	File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
		
		IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubjectEvaluation>(file,StudentClassroomSessionDivisionSubjectEvaluation.class);
		excelSheetReader.setIndex(12);
		excelSheetReader.setSheetName((String)null);
		//excelSheetReader.setFromRowIndex(1142);
		//excelSheetReader.setRowCount(22);
		OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation> setter = new OneDimensionObjectArrayAdapter<StudentClassroomSessionDivisionSubjectEvaluation>(StudentClassroomSessionDivisionSubjectEvaluation.class);
		
    	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<StudentClassroomSessionDivisionSubjectEvaluation>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public StudentClassroomSessionDivisionSubjectEvaluation instanciate(Object[] values) {
				StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation = new StudentClassroomSessionDivisionSubjectEvaluation();
				studentClassroomSessionDivisionSubjectEvaluation.setEvaluation(getEvaluation(values));
				studentClassroomSessionDivisionSubjectEvaluation.setValue(commonUtils.getBigDecimal((String)values[5]));
				
				//Subject subject = getSubject(values); //inject(SubjectDao.class).read((String)values[2]);
				//ClassroomSession classroomSession = getClassroomSession(values); //inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)values[0], (String)values[1]).iterator().next();
				//ClassroomSessionDivision classroomSessionDivision = getClassroomSessionDivision(values); //inject(ClassroomSessionDivisionDao.class).readByClassroomSessionByOrderNumber(classroomSession, 1l);
				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = getStudentClassroomSessionDivisionSubject(values); //inject(StudentClassroomSessionDivisionSubjectDao.class)
						//.readByStudentByClassroomSessionDivisionBySubject(student, classroomSessionDivision, subject);
				
				if(studentClassroomSessionDivisionSubject==null){
					Student student = getStudent(values); //inject(StudentDao.class).read((String)values[4]);
					ClassroomSessionDivisionSubject classroomSessionDivisionSubject = getClassroomSessionDivisionSubject(values); //inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionBySubject(classroomSessionDivision, subject);
					studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student, classroomSessionDivisionSubject);
					create(studentClassroomSessionDivisionSubject);
					studentClassroomSessionDivisionSubjects1.add(studentClassroomSessionDivisionSubject);
				}
				studentClassroomSessionDivisionSubjectEvaluation.setStudentClassroomSessionDivisionSubject(studentClassroomSessionDivisionSubject);
				
				return studentClassroomSessionDivisionSubjectEvaluation;
			}
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key,Object keyType) {
				return Boolean.TRUE;
			}
			
			@Override
			public StudentClassroomSessionDivisionSubjectEvaluation getInstanceByKey(Object[] values, Object key, Object type) {
				StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation = getStudentClassroomSessionDivisionSubjectEvaluation(values);
				if(studentClassroomSessionDivisionSubjectEvaluation!=null){
					studentClassroomSessionDivisionSubjectEvaluation.setStudentClassroomSessionDivisionSubject(getStudentClassroomSessionDivisionSubject(values));
					studentClassroomSessionDivisionSubjectEvaluations1Update.add(studentClassroomSessionDivisionSubjectEvaluation);
					/*if(studentClassroomSessionDivisionSubjectEvaluation.getStudentClassroomSessionDivisionSubject().getStudent().getCode().equals("IESA/2014MUJ0441-KG"))
						System.out.println(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType()+" : "
								+studentClassroomSessionDivisionSubjectEvaluation.getValue().multiply(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation()
										.getClassroomSessionDivisionSubjectEvaluationType().getWeight()));*/
				}
				
				return studentClassroomSessionDivisionSubjectEvaluation;
			}
		};
		
		excelSheetReader.execute();
		twoDimensionObjectArray.setInput(excelSheetReader.getValues());
		twoDimensionObjectArray.execute();
		
		studentClassroomSessionDivisionSubjectEvaluations1New = twoDimensionObjectArray.getOutput();
		System.out.println("#New studentClassroomSessionDivisionSubjectEvaluations : "+studentClassroomSessionDivisionSubjectEvaluations1New.size());
	
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> l = new ArrayList<>();
		int i = 0;
		long t0 = System.currentTimeMillis();
		for(StudentClassroomSessionDivisionSubjectEvaluation index : studentClassroomSessionDivisionSubjectEvaluations1New){
			if( i < 2500 ){
				l.add(index);
				i++;
			}else{
				System.out.print("Persisting...");
				long t = System.currentTimeMillis();
				create(l);
				System.out.println( ((System.currentTimeMillis()-t)/1000) );
				l.clear();
				i = 0;
			}
		}
		
		if(!l.isEmpty()){
			System.out.print("Persisting...");
			long t = System.currentTimeMillis();
			create(l);
			System.out.println( ((System.currentTimeMillis()-t)/1000) );
		}
		System.out.println( ((System.currentTimeMillis()-t0)/1000/60) );
		
		System.out.print("Updating : "+studentClassroomSessionDivisionSubjectEvaluations1Update.size()+"...");
		long t = System.currentTimeMillis();
		update(studentClassroomSessionDivisionSubjectEvaluations1Update);
		System.out.println( ((System.currentTimeMillis()-t)/1000) );
    }
    
    public void persons(){
    	File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
		
		IdentifiableExcelSheetReader<Person> excelSheetReader = new IdentifiableExcelSheetReader<Person>(file,Person.class);
		OneDimensionObjectArrayAdapter<Person> setter = new OneDimensionObjectArrayAdapter<Person>(Person.class);
		
    	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Person> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Person>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key,Object keyType) {
				return Boolean.TRUE;
			}
			
			@Override
			public Person getInstanceByKey(Object[] values, Object key, Object type) {
				Person person = getPerson(values);
				if(values[0].equals("IESA/2014MUJ0441-KG"))
					System.out.println("C : "+values[3]+" : "+person.getBirthDate());
				try {
					String[] p = StringUtils.split((String)values[3], Constant.CHARACTER_SPACE.toString());
					if(p!=null && p.length>0 && StringUtils.isNotBlank(p[0])){
						person.setBirthDate(DateUtils.parseDate(p[0], "MM/dd/yyyy"));
						if(person.getCode().equals("IESA/2014MUJ0441-KG"))
							System.out.println(person.getBirthDate());
						personsUpdated.add(person);
					}
				} catch (ParseException e) {
					System.out.println("Cannot parse : "+values[0]+" : <<"+values[3]+">>");
				}
				return person;
			}
		};
		
		excelSheetReader.execute();
		twoDimensionObjectArray.setInput(excelSheetReader.getValues());
		twoDimensionObjectArray.execute();
		
		System.out.print("Updating persons : "+personsUpdated.size()+"...");
		long t = System.currentTimeMillis();
		update(personsUpdated);
		System.out.println( ((System.currentTimeMillis()-t)/1000) );
    }
    
    private Evaluation getEvaluation(Object[] values){
		for(Evaluation evaluation : evaluations1)
			if(matchs(evaluation,values))
				return evaluation;
		return null;
	}
    
    private Boolean matchs(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType,Object[] values){
    	return classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
				,(String)values[1]) && classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject()
				.getSubject().getCode().equals(StringUtils.trim((String)values[2])) && classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getCode().equals(values[3]);
    }
    
    private Boolean matchs(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,Object[] values){
    	return classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
				,(String)values[1]) && classroomSessionDivisionSubject.getSubject().getCode().equals(StringUtils.trim((String)values[2]));
    }
    
    private Boolean matchs(ClassroomSessionDivision classroomSessionDivision,Object[] values){
    	return classroomSessionDivision.getClassroomSession().getLevelTimeDivision().getLevel().getCode().equals(values[0]) 
    			&& isSameSuffix(classroomSessionDivision.getClassroomSession(),(String)values[1]);
    }
    
    private Boolean matchs(Evaluation evaluation,Object[] values){
    	return matchs(evaluation.getClassroomSessionDivisionSubjectEvaluationType(), values);
    }
    
    private Boolean matchs(Student student,Object[] values){
    	return student.getCode().equals(values[4]);
    }
    
    private Boolean matchs(Person person,Object[] values){
    	return person.getCode().equals(values[0]);
    }
    
    private Boolean matchs(Subject subject,Object[] values){
    	return subject.getCode().equals(StringUtils.trim((String)values[2]));
    }
	
	private ClassroomSessionDivisionSubjectEvaluationType getClassroomSessionDivisionSubjectEvaluationType(Object[] values){
		for(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType : classroomSessionDivisionSubjectEvaluationTypes1)
			if(matchs(classroomSessionDivisionSubjectEvaluationType, values))
				return classroomSessionDivisionSubjectEvaluationType;
		return null;
	}
	
	private ClassroomSessionDivisionSubject getClassroomSessionDivisionSubject(Object[] values){
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects1)
			if(matchs(classroomSessionDivisionSubject.getClassroomSessionDivision(),values) && matchs(classroomSessionDivisionSubject.getSubject(),values))
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
		for(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation : studentClassroomSessionDivisionSubjectEvaluations1){
			if(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
					,(String)values[1]) && studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject()
					.getSubject().getCode().equals(StringUtils.trim((String)values[2])) && studentClassroomSessionDivisionSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals(values[3]) 
					&& studentClassroomSessionDivisionSubjectEvaluation.getStudentClassroomSessionDivisionSubject().getStudent().getCode().equals(values[4]))
				return studentClassroomSessionDivisionSubjectEvaluation;
		}
		return null;
	}
	
	private ClassroomSession getClassroomSession(Object[] values){
		for(ClassroomSession classroomSession : classroomSessions)
			if(classroomSession.getLevelTimeDivision().getLevel().getCode().equals(values[0]) && isSameSuffix(classroomSession,(String)values[1]))
				return classroomSession;
		return null;
	}
		
	private StudentClassroomSessionDivisionSubject getStudentClassroomSessionDivisionSubject(Object[] values){
		for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : studentClassroomSessionDivisionSubjects1)
			if(matchs(studentClassroomSessionDivisionSubject.getStudent(), values) && matchs(studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject(), values))
				return studentClassroomSessionDivisionSubject;
		StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(getStudent(values)
				, getClassroomSessionDivisionSubject(values));
		create(studentClassroomSessionDivisionSubject);
		studentClassroomSessionDivisionSubjects1.add(studentClassroomSessionDivisionSubject);
		//System.out.println("NF StudentClassroomSessionDivisionSubject : "+StringUtils.join(values,"|"));
		return studentClassroomSessionDivisionSubject;
	}
	
	private Person getPerson(Object[] values){
		for(Person person : persons)
			if(matchs(person, values))
				return person;
		return null;
	}
	
	private Student getStudent(Object[] values){
		for(Student student : students)
			if(matchs(student, values))
				return student;
		return null;
	}
	
	private Subject getSubject(Object[] values){
		for(Subject subject : subjects)
			if(subject.getCode().equals(StringUtils.trim((String)values[2])))
				return subject;
		return null;
	}
    /*
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer()
    			.setStructurationEnabled(Boolean.FALSE)
    			.setSynchronizationEnabled(Boolean.FALSE)
    			.setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getDivisionOrderNumbers().clear();
    	return dataProducer;
    }*/
        
}
