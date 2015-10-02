package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
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
	
	/**/
	
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
	
	public void registerStudents(String[] codes){
		for(String code : codes)
			registerStudent(code, null);
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
					assertBigDecimalEquals("Average", detail[1], studentSubject.getResults().getEvaluationSort().getAverage().getValue());
				}
		}
	}
	
	/**/
	public static SchoolBusinessTestHelper getInstance() {
		return INSTANCE;
	}
	
}
