package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.session.ClassroomSessionDivision;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @Entity
public class ClassroomSessionDivisionSubjectGroup extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSessionDivision classroomSessionDivision;
	
	@ManyToOne private SubjectGroup subjectGroup;
	
}
