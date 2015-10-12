package org.cyk.system.school.business.impl.integration;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.inject.Inject;

import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.impl.file.report.jasper.JasperReportBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.junit.Assert;

public class AverageBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;

    @Inject private StudentBusiness studentBusiness;
    
    @Deployment
    public static Archive<?> createDeployment() {
        return createRootDeployment();
    }  
    
    @Inject private StudentSubjectBusiness studentSubjectBusiness;
    @Inject private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
    @Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
    @Inject private SubjectEvaluationBusiness evaluationBusiness;
    @Inject private ClassroomSessionDivisionSubjectBusiness subjectBusiness;
    @Inject private ClassroomSessionBusiness classroomSessionBusiness;
    @Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
    
    //@Inject private StudentSubjectDao studentSubjectDao;
    //@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
    //@Inject private StudentClassroomSessionDao studentClassroomSessionDao;
    @Inject private JasperReportBusinessImpl reportBusiness;
    
    private Student student1,student2,student3;
    
    private Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
       
    private Student student(String name, String lastName){
    	Student student = new Student();
    	student.setPerson(new Person());
    	
    	student.getPerson().setName(name);
    	student.getPerson().setLastName(lastName);
    	student.getPerson().setBirthDate(new Date());
    
    	//student.getPerson().setImage(new File());
    	//student.getPerson().getImage().setBytes(RandomDataProvider.getInstance().getMale().photo());
    	//student.getPerson().getImage().setExtension("png");
    	//create(student.getPerson().getImage());
    	
    	studentBusiness.create(student);
    	return student;
    }
        
    private SubjectEvaluation evaluate(ClassroomSessionDivisionSubject subject,SubjectEvaluationType type, Object[] objects){
    	SubjectEvaluation evaluation = new SubjectEvaluation();
    	//evaluation.setSubject(subject);
    	//evaluation.setType(type);
    	//evaluation.setEvaluatedStudents(new ArrayList<StudentSubjectEvaluation>());
    	for(int i=0;i<objects.length;i=i+2){
    		Student student = (Student)objects[i];
    		StudentClassroomSession studentClassroomSession = studentClassroomSessionBusiness.finddByStudentByClassroomSession(student, subject.getClassroomSessionDivision().getClassroomSession());
    		if(studentClassroomSession==null){
    			studentClassroomSession = new StudentClassroomSession(student, subject.getClassroomSessionDivision().getClassroomSession());
    			studentClassroomSessionBusiness.create(studentClassroomSession);
    		}
    		StudentClassroomSessionDivision studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.findByStudentByClassroomSessionDivision(student,subject.getClassroomSessionDivision());
    		if(studentClassroomSessionDivision==null){
    			studentClassroomSessionDivision = new StudentClassroomSessionDivision(student,subject.getClassroomSessionDivision());
    			studentClassroomSessionDivisionBusiness.create(studentClassroomSessionDivision);
    			
    		}
    		StudentSubject studentSubject = studentSubjectBusiness.findByStudentBySubject(student, subject);
    		if(studentSubject==null){
    			studentSubject = new StudentSubject(student, subject);
    			studentSubjectBusiness.create(studentSubject);		
    		}
    		//evaluation.getEvaluatedStudents().add(new StudentSubjectEvaluation(studentSubject,new BigDecimal((String)objects[i+1])));
    	}
    	return evaluationBusiness.create(evaluation);
    }
    
    private void lecture(ClassroomSessionDivisionSubject subject,Integer minutes,Object[] objects){
    	Date now = new Date();
    	Event event = new Event(null, null, subject.getSubject().getName(), null, new Period(now, DateUtils.addMinutes(now, minutes)));
    	event.setContactCollection(null);
    	create(event);
    	Lecture lecture = new Lecture(subject,event);
    	create(lecture);
    	for(int i=0;i<objects.length;i=i+3){
    		Student student = (Student) objects[i];
    		EventParticipation eventParticipation = new EventParticipation(student.getPerson(), event);
    		create(eventParticipation);
    		if(Boolean.TRUE.equals(objects[i+1])){
    			EventMissed eventMissed = new EventMissed(eventParticipation,null,(Long)objects[i+2]);
    			create(eventMissed);
    		}
    	}
    }
    
    @Override
    protected void finds() {
        
    }
    
    @Override
    protected void businesses() {
    	installApplication();
        
        RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
        
        student1 = student("Aboua", "Paul");
    	student2 = student("Comoe", "Eric");
    	student3 = student("Nandou", "Jean");
    	
    	List<ClassroomSession> classroomSessions = (List<ClassroomSession>) classroomSessionBusiness.find().all();
    	List<ClassroomSessionDivision> classroomSessionDivisions = (List<ClassroomSessionDivision>) classroomSessionDivisionBusiness.find().all();
    	
    	List<SubjectEvaluationType> evts = new ArrayList<>(); 
    	for(AbstractIdentifiable identifiable : genericBusiness.use(SubjectEvaluationType.class).find().all())
    		evts.add((SubjectEvaluationType) identifiable);
    	List<ClassroomSessionDivisionSubject> csds = (List<ClassroomSessionDivisionSubject>) subjectBusiness.find().all();
    	
    	ClassroomSession classroomSession = classroomSessions.get(0);
    	ClassroomSessionDivision classroomSessionDivision1=classroomSessionDivisions.get(0),classroomSessionDivision2=classroomSessionDivisions.get(1)
    			,classroomSessionDivision3=classroomSessionDivisions.get(2);
    	ClassroomSessionDivisionSubject subjectMathsClassroomSessionDivision1=csds.get(0),subjectEnglishClassroomSessionDivision1=csds.get(1),
    		subjectMathsClassroomSessionDivision2=csds.get(2),subjectEnglishClassroomSessionDivision2=csds.get(3);
    	SubjectEvaluationType interro=evts.get(0),devoir=evts.get(1);
    	
    	evaluate(subjectMathsClassroomSessionDivision1,interro, new Object[]{student1, "5",student2, "3"});
    	evaluate(subjectMathsClassroomSessionDivision1,devoir, new Object[]{student1, "9",student2, "13",student3, "10"});
    	
    	evaluate(subjectEnglishClassroomSessionDivision1,interro, new Object[]{student1, "15",student2, "5",student3, "10"});
    	evaluate(subjectEnglishClassroomSessionDivision1,interro, new Object[]{student1, "5",student2, "3",student3, "17"});
    	evaluate(subjectEnglishClassroomSessionDivision1,devoir, new Object[]{student1, "7",student2, "11",student3, "9"});
    	    	
    	evaluate(subjectMathsClassroomSessionDivision2,interro, new Object[]{student1, "12",student2, "8",student3,"17"});
    	evaluate(subjectMathsClassroomSessionDivision2,devoir, new Object[]{student1, "22",student2, "26",student3, "20"});
    	
    	evaluate(subjectEnglishClassroomSessionDivision2,interro, new Object[]{student1, "10",student2, "11",student3, "8.5"});
    	evaluate(subjectEnglishClassroomSessionDivision2,interro, new Object[]{student1, "12",student2, "9",student3, "13.75"});
    	evaluate(subjectEnglishClassroomSessionDivision2,devoir, new Object[]{student1, "24",student2, "27",student3, "18"});
    	
    	lecture(subjectEnglishClassroomSessionDivision1,60, new Object[]{student1,Boolean.FALSE,0,student2,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*30,student3,Boolean.FALSE,0});
    	lecture(subjectEnglishClassroomSessionDivision1,60, new Object[]{student1,Boolean.FALSE,0,student2,Boolean.FALSE,0,student3,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*60});
    	
    	lecture(subjectMathsClassroomSessionDivision1,90, new Object[]{student1,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*45,student2,Boolean.FALSE,0,student3,Boolean.FALSE,0});
    	
    	lecture(subjectEnglishClassroomSessionDivision2,120, new Object[]{student1,Boolean.FALSE,0,student2,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*60,student3,Boolean.FALSE,0});
    	
    	lecture(subjectMathsClassroomSessionDivision2,120, new Object[]{student1,Boolean.FALSE,0,student2,Boolean.FALSE,0,student3,Boolean.FALSE,0});
    	lecture(subjectMathsClassroomSessionDivision2,60, new Object[]{student1,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*60,student2,Boolean.TRUE,DateUtils.MILLIS_PER_MINUTE*15,student3,Boolean.FALSE,0});
        
       
        
        assertStudentSubjectAverage(subjectMathsClassroomSessionDivision1 ,rankOptions,new Object[]{student1,"4.66",3,student2,"5.33",1,student3,"5.00",2});
        assertStudentSubjectAverage(subjectEnglishClassroomSessionDivision1 ,rankOptions,new Object[]{student1,"6.75",2,student2,"4.75",3,student3,"9.00",1});
        
        assertStudentSubjectAverage(subjectMathsClassroomSessionDivision2 ,rankOptions,new Object[]{student1,"11.33",2,student2,"11.33",2,student3,"12.33",1});
        assertStudentSubjectAverage(subjectEnglishClassroomSessionDivision2 ,rankOptions,new Object[]{student1,"11.50",2,student2,"11.75",1,student3,"10.06",3});
        
        assertStudentClassroomSessionDivisionAverage(classroomSessionDivision1 ,rankOptions,new Object[]{student1,"5.70",2,student2,"5.04",3,student3,"7.00",1});
        assertStudentClassroomSessionDivisionAverage(classroomSessionDivision2 ,rankOptions,new Object[]{student1,"11.40",2,student2,"11.51",1,student3,"11.35",3});
        
        assertStudentClassroomSessionAverage(classroomSession ,rankOptions,new Object[]{student1,"9.50",2,student2,"9.35",3,student3,"9.90",1});
        
        assertStudentSubjectAttendance(subjectEnglishClassroomSessionDivision1, new Object[]{student1,120l,0l,0l,student2,90l,30l,0l,student3,60l,60l,0l});
        assertStudentSubjectAttendance(subjectMathsClassroomSessionDivision1,new Object[]{student1,45l,45l,0l,student2,90l,0l,0l,student3,90l,0l,0l});
        
        assertStudentSubjectAttendance(subjectEnglishClassroomSessionDivision2,new Object[]{student1,120l,0l,0l,student2,60l,60l,0l,student3,120l,0l,0l});
        assertStudentSubjectAttendance(subjectMathsClassroomSessionDivision2,new Object[]{student1,120l,60l,0l,student2,165l,15l,0l,student3,180l,0l,0l});
        
        assertStudentClassroomSessionDivisionAttendance(classroomSessionDivision1,new Object[]{student1,165l,45l,0l,student2,180l,30l,0l,student3,150l,60l,0l});
        assertStudentClassroomSessionDivisionAttendance(classroomSessionDivision2,new Object[]{student1,240l,60l,0l,student2,225l,75l,0l,student3,300l,0l,0l});
        
        //ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> report = studentClassroomSessionDivisionBusiness.buildReport(
        //		Arrays.asList(classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3), Boolean.FALSE);
        //reportBusiness.write(new java.io.File("h:\\"), report);
        
    }
    
    /**/
    
    private void assertStudentAverage(Student student,StudentResults results,BigDecimal average,Integer rank){
    	Assert.assertEquals("Average - "+student,average,results.getEvaluationSort().getAverage().getValue());
		Assert.assertEquals("Rank - "+student,rank,results.getEvaluationSort().getRank().getValue());
    }
    
    private void assertStudentAttendance(Student student,StudentResults results,Long attended,Long missed,Long justified){
    	Assert.assertEquals("Attended - "+student,attended.longValue(),results.getLectureAttendance().getAttendedDuration().longValue());
		Assert.assertEquals("Missed - "+student,missed.longValue(),results.getLectureAttendance().getMissedDuration().longValue());
		Assert.assertEquals("Justified - "+student,justified.longValue(),results.getLectureAttendance().getMissedDurationJustified().longValue());
    }
    
    private void assertStudentSubjectAverage(ClassroomSessionDivisionSubject subject,RankOptions<SortableStudentResults> rankOptions,Object...expecteds){
    	Collection<StudentSubject> studentSubjects = studentSubjectBusiness.average(Arrays.asList(subject),Boolean.FALSE);
    	studentSubjectBusiness.rank(studentSubjects,rankOptions);
    	for(int i=0;i<expecteds.length;i=i+3){
    		Student student = (Student) expecteds[i];
    		for(StudentSubject studentSubject : studentSubjects)
    			if(studentSubject.getStudent().equals(student))
    				assertStudentAverage(student, studentSubject.getResults(), new BigDecimal((String)expecteds[i+1]), (Integer)expecteds[i+2]);
    				
    	}
    }
    
    private void assertStudentClassroomSessionDivisionAverage(ClassroomSessionDivision classroomSessionDivision,RankOptions<SortableStudentResults> rankOptions,Object...expecteds){
    	Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = studentClassroomSessionDivisionBusiness.average(Arrays.asList(classroomSessionDivision),Boolean.FALSE);
    	studentClassroomSessionDivisionBusiness.rank(studentClassroomSessionDivisions,rankOptions);
    	for(int i=0;i<expecteds.length;i=i+3){
    		Student student = (Student) expecteds[i];
    		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
    			if(studentClassroomSessionDivision.getStudent().equals(student))
    				assertStudentAverage(student, studentClassroomSessionDivision.getResults(), new BigDecimal((String)expecteds[i+1]), (Integer)expecteds[i+2]);
    				
    			this.studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
    		}
    	}
    } 
    
    private void assertStudentClassroomSessionAverage(ClassroomSession classroomSession,RankOptions<SortableStudentResults> rankOptions,Object...expecteds){
    	Collection<StudentClassroomSession> studentClassroomSessions = studentClassroomSessionBusiness.average(Arrays.asList(classroomSession),Boolean.FALSE);
    	studentClassroomSessionBusiness.rank(studentClassroomSessions,rankOptions);
    	for(int i=0;i<expecteds.length;i=i+3){
    		Student student = (Student) expecteds[i];
    		for(StudentClassroomSession studentClassroomSession : studentClassroomSessions)
    			if(studentClassroomSession.getStudent().equals(student))
    				assertStudentAverage(student, studentClassroomSession.getResults(), new BigDecimal((String)expecteds[i+1]), (Integer)expecteds[i+2]);
    				
    	}
    }
    
    /**/
    
    private void assertStudentSubjectAttendance(ClassroomSessionDivisionSubject subject,Object...expecteds){
    	Collection<StudentSubject> studentSubjects = studentSubjectBusiness.attendance(Arrays.asList(subject));
    	for(int i=0;i<expecteds.length;i=i+4){
    		Student student = (Student) expecteds[i];
    		for(StudentSubject studentSubject : studentSubjects)
    			if(studentSubject.getStudent().equals(student))
    				assertStudentAttendance(student, studentSubject.getResults(), ((Long)expecteds[i+1])*DateUtils.MILLIS_PER_MINUTE, ((Long)expecteds[i+2])*DateUtils.MILLIS_PER_MINUTE, ((Long)expecteds[i+3])*DateUtils.MILLIS_PER_MINUTE);
    				
    	}
    }
    
    private void assertStudentClassroomSessionDivisionAttendance(ClassroomSessionDivision classroomSessionDivision,Object...expecteds){
    	Collection<StudentClassroomSessionDivision> studentClassroomDivisionSessions = studentClassroomSessionDivisionBusiness.attendance(Arrays.asList(classroomSessionDivision));
    	for(int i=0;i<expecteds.length;i=i+4){
    		Student student = (Student) expecteds[i];
    		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomDivisionSessions)
    			if(studentClassroomSessionDivision.getStudent().equals(student))
    				assertStudentAttendance(student, studentClassroomSessionDivision.getResults(), ((Long)expecteds[i+1])*DateUtils.MILLIS_PER_MINUTE, ((Long)expecteds[i+2])*DateUtils.MILLIS_PER_MINUTE, ((Long)expecteds[i+3])*DateUtils.MILLIS_PER_MINUTE);
    			
    	}
    }

}
