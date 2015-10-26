package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.LectureDao;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationDao;

@Stateless
public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private SubjectEvaluationDao subjectEvaluationDao;
	@Inject private LectureDao lectureDao;
	
	@Inject
	public ClassroomSessionDivisionSubjectBusinessImpl(ClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}
    
	@Override
	protected void __load__(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.__load__(classroomSessionDivisionSubject);
		classroomSessionDivisionSubject.setEvaluations(subjectEvaluationDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		classroomSessionDivisionSubject.setLectures(lectureDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
	}
	
}
