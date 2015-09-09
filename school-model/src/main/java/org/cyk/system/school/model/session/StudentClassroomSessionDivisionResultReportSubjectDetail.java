package org.cyk.system.school.model.session;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class StudentClassroomSessionDivisionResultReportSubjectDetail implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private StudentClassroomSessionDivisionResultReport report;
	private String name,average,coefficient,averageCoefficiented,rank,appreciation,teacherNames;
	
}