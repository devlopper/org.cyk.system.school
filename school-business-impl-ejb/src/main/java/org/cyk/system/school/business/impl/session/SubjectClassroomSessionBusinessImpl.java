package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.session.SubjectClassroomSessionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class SubjectClassroomSessionBusinessImpl extends AbstractTypedBusinessService<SubjectClassroomSession, SubjectClassroomSessionDao> implements SubjectClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public SubjectClassroomSessionBusinessImpl(SubjectClassroomSessionDao dao) {
		super(dao); 
	}
	
	@Override
	public SubjectClassroomSession create(SubjectClassroomSession subjectClassroomSession) {
		super.create(subjectClassroomSession);
		Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionDao.class).readByClassroomSession(subjectClassroomSession.getClassroomSession());
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision, subjectClassroomSession.getSubject()
					, subjectClassroomSession.getWeight(), subjectClassroomSession.getTeacher());
			
			inject(ClassroomSessionDivisionSubjectBusiness.class).create(classroomSessionDivisionSubject);
		}
		return subjectClassroomSession;
	}
	
	@Override
	public SubjectClassroomSession update(SubjectClassroomSession subjectClassroomSession) {
		SubjectClassroomSession oldSubjectClassroomSession = dao.read(subjectClassroomSession.getIdentifier());
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionBySubject(oldSubjectClassroomSession.getClassroomSession(), oldSubjectClassroomSession.getSubject())){
			classroomSessionDivisionSubject.setSubject(subjectClassroomSession.getSubject());
			if(Boolean.TRUE.equals(subjectClassroomSession.getCascadeOperationToChildren())){
				classroomSessionDivisionSubject.setTeacher(subjectClassroomSession.getTeacher());
				classroomSessionDivisionSubject.setWeight(subjectClassroomSession.getWeight());
				
			}
			inject(ClassroomSessionDivisionSubjectDao.class).update(classroomSessionDivisionSubject);
		}
		return super.update(subjectClassroomSession);
	}
	
	@Override
	public SubjectClassroomSession delete(SubjectClassroomSession subjectClassroomSession) {
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionBySubject(subjectClassroomSession.getClassroomSession(), subjectClassroomSession.getSubject()))
			inject(ClassroomSessionDivisionSubjectBusiness.class).delete(classroomSessionDivisionSubject);
		return super.delete(subjectClassroomSession);
	}

	@Override
	public Collection<SubjectClassroomSession> findBySubject(Subject subject) {
		return dao.readBySubject(subject);
	}

	@Override
	public Collection<SubjectClassroomSession> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}

	@Override
	public SubjectClassroomSession findBySubjectByClassroomSession(Subject subject, ClassroomSession classroomSession) {
		return dao.readBySubjectByClassroomSession(subject, classroomSession);
	}

	@Override
	public Collection<SubjectClassroomSession> findByClassroomSessionByStudent(ClassroomSession classroomSession,Student student) {
		return dao.readByClassroomSessionByStudent(classroomSession,student);
	}

	
	
}
