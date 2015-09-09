package org.cyk.system.school.model.session;

import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionResultReport implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;

	private InputStream schoolLogo,photo;
	
	private String schoolName,orderNumber,academicSession,names,classroomSession,dateOfBirth,title,
	totalMissedHours,totalMissedHoursJustified,average,rank,numberOfStudents,classroomSessionAverage,
	classroomSessionAverageHighest,classroomSessionAverageLowest,appreciation,appreciationComments,
	appreciationCommentedBy,staffTitle,staffPerson,signatureInfos,totalAverage,totalCoefficient,totalAverageCoefficiented,footer,governmentMinistryInfos;
	
	private Collection<StudentClassroomSessionDivisionResultReportSubjectDetail> subjects = new ArrayList<>();
	
}
