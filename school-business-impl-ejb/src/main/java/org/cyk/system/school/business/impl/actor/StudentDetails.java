package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;

import org.cyk.system.root.business.impl.party.person.AbstractActorDetails;
import org.cyk.system.school.model.actor.Student;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class StudentDetails extends AbstractActorDetails.AbstractDefault<Student> implements Serializable {

	private static final long serialVersionUID = 1L;

	private String admissionLevelTimeDivision,classroomSession;
	
	public StudentDetails(Student student) {
		super(student);
		admissionLevelTimeDivision = formatUsingBusiness(student.getAdmissionLevelTimeDivision());
	}

	public static final String FIELD_ADMISSION_LEVEL_TIME_DIVISION = "admissionLevelTimeDivision";
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	
}
