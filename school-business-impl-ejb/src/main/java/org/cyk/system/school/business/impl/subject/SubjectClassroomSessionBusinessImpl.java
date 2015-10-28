package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.SubjectClassroomSessionBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;

public class SubjectClassroomSessionBusinessImpl extends AbstractTypedBusinessService<SubjectClassroomSession, SubjectClassroomSessionDao> implements SubjectClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public SubjectClassroomSessionBusinessImpl(SubjectClassroomSessionDao dao) {
		super(dao); 
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

	
	
}
