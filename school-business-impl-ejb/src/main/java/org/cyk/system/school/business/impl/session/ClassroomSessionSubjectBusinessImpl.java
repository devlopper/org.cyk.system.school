package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;

public class ClassroomSessionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionSubject, ClassroomSessionSubjectDao> implements ClassroomSessionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionSubjectBusinessImpl(ClassroomSessionSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSessionSubject classroomSessionSubject, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSessionSubject.getClassroomSession(),classroomSessionSubject.getSubject()};
		return super.getPropertyValueTokens(classroomSessionSubject, name);
	}
	
	@Override
	public ClassroomSessionSubject instanciateOne(ClassroomSession classroomSession, Subject subject) {
		ClassroomSessionSubject classroomSessionSubject = instanciateOne();
		classroomSessionSubject.setSubject(subject);
		classroomSessionSubject.setClassroomSession(classroomSession);
		return classroomSessionSubject;
	}
	
	@Override
	protected void afterCreate(ClassroomSessionSubject classroomSessionSubject) {
		super.afterCreate(classroomSessionSubject);		
		if(Boolean.TRUE.equals(classroomSessionSubject.getCascadeOperationToChildren())){
			Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionDao.class).readByClassroomSession(classroomSessionSubject.getClassroomSession());
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
				ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision, classroomSessionSubject.getSubject()
						, classroomSessionSubject.getWeight(), classroomSessionSubject.getTeacher());
				inject(ClassroomSessionDivisionSubjectBusiness.class).create(classroomSessionDivisionSubject);
			}	
		}
	}
	
	@Override
	protected void beforeUpdate(ClassroomSessionSubject classroomSessionSubject) {
		super.beforeUpdate(classroomSessionSubject);
		ClassroomSessionSubject oldClassroomSessionSubject = dao.read(classroomSessionSubject.getIdentifier());
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionBySubject(oldClassroomSessionSubject.getClassroomSession(), oldClassroomSessionSubject.getSubject())){
			if(Boolean.TRUE.equals(classroomSessionSubject.getCascadeOperationToChildren())){
				classroomSessionDivisionSubject.setTeacher(classroomSessionSubject.getTeacher());
				classroomSessionDivisionSubject.setWeight(classroomSessionSubject.getWeight());
			}
			inject(ClassroomSessionDivisionSubjectDao.class).update(classroomSessionDivisionSubject);
		}
	}
		
	@Override
	protected void beforeDelete(ClassroomSessionSubject classroomSessionSubject) {
		super.beforeDelete(classroomSessionSubject);
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : inject(ClassroomSessionDivisionSubjectDao.class).readByClassroomSessionBySubject(classroomSessionSubject.getClassroomSession(), classroomSessionSubject.getSubject()))
			inject(ClassroomSessionDivisionSubjectBusiness.class).delete(classroomSessionDivisionSubject);
	}

	@Override
	public Collection<ClassroomSessionSubject> findBySubject(Subject subject) {
		return dao.readBySubject(subject);
	}

	@Override
	public Collection<ClassroomSessionSubject> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}

	@Override
	public ClassroomSessionSubject findByClassroomSessionBySubject(ClassroomSession classroomSession, Subject subject) {
		return dao.readByClassroomSessionBySubject(classroomSession, subject);
	}

	@Override
	public Collection<ClassroomSessionSubject> findByClassroomSessionByStudent(ClassroomSession classroomSession,Student student) {
		return dao.readByClassroomSessionByStudent(classroomSession,student);
	}

	@Override
	public ClassroomSessionSubject instanciateOne(String[] values) {
		ClassroomSessionSubject classroomSessionSubject = instanciateOne();
		Integer index = 0;
		classroomSessionSubject.setSubject(inject(SubjectDao.class).read(values[index++]));
		String classroomSessionCode = values[index++];
		if(StringUtils.isNotBlank(classroomSessionCode))
			classroomSessionSubject.setClassroomSession(inject(ClassroomSessionDao.class).read(classroomSessionCode));
		return classroomSessionSubject;
	}
	
}
