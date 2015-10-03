package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;

@Singleton
public class SchoolBusinessTestHelper extends AbstractTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentSubjectEvaluationBusiness studentSubjectEvaluationBusiness;
	@Inject private SubjectEvaluationBusiness subjectEvaluationBusiness;
	@Inject private SubjectEvaluationTypeBusiness evaluationTypeBusiness;
	
	@Getter @Setter private Boolean coefficientApplied = Boolean.TRUE;
	@Getter @Setter private RankOptions<SortableStudentResults> rankOptions;
	
	/**/
	
	@Override
	protected void initialisation() {
		super.initialisation();
		rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
	}
	
	public Student registerStudent(String code,String[] names){
		Student student = RootRandomDataProvider.getInstance().actor(Student.class);
		student.getRegistration().setCode(code);
		if(names!=null){
			if(names.length>0)
				student.getPerson().setName(names[0]);
			if(names.length>1)
				student.getPerson().setLastName(names[1]);
			if(names.length>2)
				student.getPerson().setSurname(names[2]);
		}
		return studentBusiness.create(student);
	}
	
	public Collection<Student> registerStudents(String[] codes){
		Collection<Student> students = new ArrayList<>();
		for(String code : codes)
			students.add(registerStudent(code, null));
		return students;
	}
	
	public void takeSubjects(String[] studentRegistrationCodes,ClassroomSessionDivisionSubject[] classroomSessionDivisionSubjects){
		for(String studentRegistrationCode : studentRegistrationCodes){
			Student student = studentBusiness.findByRegistrationCode(studentRegistrationCode);
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
				StudentSubject studentSubject = new StudentSubject(student, classroomSessionDivisionSubject);
				studentSubjectBusiness.create(studentSubject);
			}
		}
	}
	
	public void evaluateStudents(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName,Boolean coefficientApplied,String[][] details){
		SubjectEvaluation subjectEvaluation = new SubjectEvaluation(evaluationTypeBusiness.findBySubjectByEvaluationType(subject, evaluationTypeName),coefficientApplied);
		for(String[] detail : details){
			Student student = studentBusiness.findByRegistrationCode(detail[0]);
			StudentSubject studentSubject = studentSubjectBusiness.findByStudentBySubject(student, subjectEvaluation.getType().getSubject());
			subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation,studentSubject, new BigDecimal(detail[1])));
		}
		subjectEvaluationBusiness.create(subjectEvaluation);
		
		//System.out.println(studentSubjectEvaluationBusiness.findAll());
	}
	public void evaluateStudents(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName,String[][] details){
		evaluateStudents(subject, evaluationTypeName, coefficientApplied,details);
	}
	
	/**/
	
	public void assertClassroomSessionDivisionSubjectAverage(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details){
		Collection<StudentSubject> studentSubjects = studentSubjectBusiness.average(Arrays.asList(classroomSessionDivisionSubject), Boolean.TRUE);
		for(StudentSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getRegistration().getCode())){
					assertBigDecimalEquals("Average of "+studentSubject.getStudent(), detail[1], studentSubject.getResults().getEvaluationSort().getAverage().getValue());
				}
		}
	}
	
	public void assertClassroomSessionDivisionSubjectRank(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,RankOptions<SortableStudentResults> rankOptions){
		Collection<StudentSubject> studentSubjects = studentSubjectBusiness.average(Arrays.asList(classroomSessionDivisionSubject), Boolean.TRUE);
		studentSubjectBusiness.rank(studentSubjects,rankOptions);
		for(StudentSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getRegistration().getCode())){
					assertEquals("Rank Value of "+studentSubject.getStudent(), detail[1], studentSubject.getResults().getEvaluationSort().getRank().getValue().toString());
					assertEquals("Rank Exaequo of "+studentSubject.getStudent(), detail.length>2?detail[2]:"false", 
							studentSubject.getResults().getEvaluationSort().getRank().getExaequo()==null?"false":studentSubject.getResults().getEvaluationSort().getRank().getExaequo());
				}
		}
	}
	
	public void assertClassroomSessionDivisionSubjectAfterEvaluation(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType, String[][] details,RankOptions<SortableStudentResults> rankOptions){
		evaluateStudents(classroomSessionDivisionSubject, evaluationType,extract(details, 1));
    	assertClassroomSessionDivisionSubjectAverage(classroomSessionDivisionSubject, extract(details, 2));    	
    	assertClassroomSessionDivisionSubjectRank(classroomSessionDivisionSubject,extract(details, 3),rankOptions);
	}
	
	public void assertClassroomSessionDivisionSubjectAfterEvaluation(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType, String[][] details){
		assertClassroomSessionDivisionSubjectAfterEvaluation(classroomSessionDivisionSubject, evaluationType, details, rankOptions);
	}
	
	private String[][] extract(String[][] details,Integer columnIndex){
		String[][] data = new String[details.length][2];
		for(int rowIndex = 0;rowIndex<details.length;rowIndex++){
			data[rowIndex][0] = details[rowIndex][0];
			data[rowIndex][1] = details[rowIndex][columnIndex];
		}
		return data;
	}
	
	/**/
	public static SchoolBusinessTestHelper getInstance() {
		return INSTANCE;
	}
	
}
