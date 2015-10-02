package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface LectureDao extends TypedDao<Lecture> {

	Collection<Lecture> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects);

	Collection<Lecture> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> levels);

	Collection<Lecture> readByClassroomSessions(Collection<ClassroomSession> levels);
	
	Collection<Event> readEvents(Collection<Lecture> lectures);
	
}
