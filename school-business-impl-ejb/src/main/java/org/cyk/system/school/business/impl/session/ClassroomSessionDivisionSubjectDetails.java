package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionSubjectDetails extends AbstractOutputDetails<ClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String name,coefficient,teacher;
	
	public ClassroomSessionDivisionSubjectDetails(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super(classroomSessionDivisionSubject);
		name = classroomSessionDivisionSubject.getSubject().getName();
		coefficient = numberBusiness.format(classroomSessionDivisionSubject.getCoefficient());
		if(classroomSessionDivisionSubject.getTeacher()!=null)
			teacher = classroomSessionDivisionSubject.getTeacher().getPerson().getNames();
	}
}