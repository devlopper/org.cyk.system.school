package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

@Stateless
public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private SubjectClassroomSessionDao subjectClassroomSessionDao;
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public ClassroomSessionDivisionSubjectBusinessImpl(ClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	public ClassroomSessionDivisionSubject create(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.create(classroomSessionDivisionSubject);
		SubjectClassroomSession subjectClassroomSession = subjectClassroomSessionDao.readBySubjectByClassroomSession(
				classroomSessionDivisionSubject.getSubject(),classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession());
		if(subjectClassroomSession==null){
			subjectClassroomSession = new SubjectClassroomSession(classroomSessionDivisionSubject.getSubject(),classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession());
			subjectClassroomSessionDao.create(subjectClassroomSession);
		}
		commonUtils.increment(Long.class, classroomSessionDivisionSubject.getClassroomSessionDivision(), ClassroomSessionDivision.FIELD_NUMBER_OF_SUBJECTS, 1l);
		classroomSessionDivisionDao.update(classroomSessionDivisionSubject.getClassroomSessionDivision());
		return classroomSessionDivisionSubject;
	}
	
	private void cascade(ClassroomSessionDivisionSubject classroomSessionDivisionSubject){
		
	}
	
	@Override
	public ClassroomSessionDivisionSubject delete(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		cascade(classroomSessionDivisionSubject);
		return super.delete(classroomSessionDivisionSubject);
	}
    
	//TODO should be factored in higher class
	@Override
	public void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentClassroomSessionDivisionSubject> studentSubjects) {
		logTrace("Computing node results of {} Classroom Session Division Subject(s). Number of students={}", classroomSessionDivisionSubjects.size(),studentSubjects.size());
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			NodeResults results = classroomSessionDivisionSubject.getResults();
			results.setAverageLowest(BigDecimal.ZERO);
			results.setAverage(BigDecimal.ZERO);
			results.setAverageHighest(BigDecimal.ZERO);
			results.setNumberOfStudentPassingEvaluationAverage(0);
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
				if(studentSubject.getClassroomSessionDivisionSubject().getIdentifier().equals(classroomSessionDivisionSubject.getIdentifier())){
					BigDecimal value = studentSubject.getResults().getEvaluationSort().getAverage().getValue();
					if(value==null){
						
					}else{
						weightedValues.add(new WeightedValue(value,BigDecimal.ONE));
						if(value.compareTo(results.getAverageLowest())==-1){
							results.setAverageLowest(value);
						}
						if(value.compareTo(results.getAverageHighest())==1){
							results.setAverageHighest(value);
						}
						//TODO should be take first on subject if null on higher
						if(value.compareTo(studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession().getAcademicSession()
								.getNodeInformations().getEvaluationPassAverage())>=0){
							results.setNumberOfStudentPassingEvaluationAverage(results.getNumberOfStudentPassingEvaluationAverage()+1);
						}
					}
				}else{
					
				}
			}
			results.setNumberOfStudent(weightedValues.size());
			results.setAverage(RootBusinessLayer.getInstance().getMathematicsBusiness().average(weightedValues, null, null).getValue());
			logIdentifiable("Average computed", classroomSessionDivisionSubject);
			dao.update(classroomSessionDivisionSubject);
		}
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher) {
		return dao.readByClassroomSessionDivisionByTeacher(classroomSessionDivision,teacher);
	}

}
