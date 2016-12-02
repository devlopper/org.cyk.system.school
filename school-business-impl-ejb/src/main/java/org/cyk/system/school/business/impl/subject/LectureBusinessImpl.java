package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.api.event.EventBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.persistence.api.subject.LectureDao;

public class LectureBusinessImpl extends AbstractTypedBusinessService<Lecture, LectureDao> implements LectureBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LectureBusinessImpl(LectureDao dao) {
		super(dao); 
	}

	@Override
	public Lecture create(Lecture lecture) {
		if(lecture.getEvent().getIdentifier()==null)
			inject(EventBusiness.class).create(lecture.getEvent());
		return super.create(lecture);
	}
	
}
