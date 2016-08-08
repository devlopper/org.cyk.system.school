package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class LectureDetails extends AbstractOutputDetails<Lecture> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String date;
	
	public LectureDetails(Lecture lecture) {
		super(lecture);
		date = timeBusiness.formatDate(lecture.getEvent().getExistencePeriod().getFromDate());
	}
}