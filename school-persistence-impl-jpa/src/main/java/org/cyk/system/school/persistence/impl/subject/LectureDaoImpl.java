package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.LectureDao;

public class LectureDaoImpl extends AbstractTypedDao<Lecture> implements LectureDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readBySubjects,readByClassroomSessionDivisions,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn(Lecture.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT));
        
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(commonUtils.attributePath(Lecture.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT
        		, StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION)));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(Lecture.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT
        		, StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION,StudentClassroomSession.FIELD_CLASSROOM_SESSION)));
    }
    
	@Override
	public Collection<Lecture> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects) {
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
	public Collection<Lecture> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return readBySubjects(Arrays.asList(classroomSessionDivisionSubject));
	}

	@Override
	public Collection<Event> readEvents(Collection<Lecture> lectures) {
		Collection<Event> events = new HashSet<>();
		for(Lecture lecture : lectures)
			events.add(lecture.getEvent());
		return events;
	}
	
}
 