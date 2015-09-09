package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;

import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.LectureDao;

public class LectureDaoImpl extends AbstractTypedDao<Lecture> implements LectureDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readBySubjects,readByClassroomSessionDivisions,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn("subject"));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn("subject.classroomSessionDivision"));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn("subject.classroomSessionDivision.classroomSession"));
    }
    
	@Override
	public Collection<Lecture> readBySubjects(Collection<Subject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<Lecture> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> levels) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(levels).resultMany();
	}

	@Override
	public Collection<Lecture> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

	@Override
	public Collection<Event> readEvents(Collection<Lecture> lectures) {
		Collection<Event> events = new HashSet<>();
		for(Lecture lecture : lectures)
			events.add(lecture.getEvent());
		return events;
	}
	
}
 