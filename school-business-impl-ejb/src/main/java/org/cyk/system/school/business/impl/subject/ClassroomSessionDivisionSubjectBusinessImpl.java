package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.LectureDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;

@Stateless
public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private EvaluationDao subjectEvaluationDao;
	@Inject private LectureDao lectureDao;
	@Inject SubjectClassroomSessionDao subjectClassroomSessionDao;
	
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
		return classroomSessionDivisionSubject;
	}
	
	private void cascade(ClassroomSessionDivisionSubject classroomSessionDivisionSubject){
		
	}
	
	@Override
	public ClassroomSessionDivisionSubject delete(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		cascade(classroomSessionDivisionSubject);
		return super.delete(classroomSessionDivisionSubject);
	}
    
	@Override
	protected void __load__(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.__load__(classroomSessionDivisionSubject);
		classroomSessionDivisionSubject.setEvaluations(subjectEvaluationDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		classroomSessionDivisionSubject.setLectures(lectureDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
	}
	
	@Override
	public void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentSubject> studentSubjects) {
		logTrace("Computing node results of {} Classroom Session Division Subject(s). Number of students={}", classroomSessionDivisionSubjects.size(),studentSubjects.size());
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			NodeResults results = classroomSessionDivisionSubject.getResults();
			results.setAverageLowest(BigDecimal.ZERO);
			results.setAverage(BigDecimal.ZERO);
			results.setAverageHighest(BigDecimal.ZERO);
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			for(StudentSubject studentSubject : studentSubjects){
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

	@Override
	public Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
}
