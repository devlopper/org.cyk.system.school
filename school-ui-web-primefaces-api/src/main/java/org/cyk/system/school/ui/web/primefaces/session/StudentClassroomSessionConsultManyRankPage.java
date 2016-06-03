package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.web.api.WebManager;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultManyRankPage extends AbstractStudentClassroomSessionConsultManyRankPage implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected Collection<StudentClassroomSession> getStudentClassroomSessions() {
		Collection<Long> identifiers = WebManager.getInstance().decodeIdentifiersRequestParameter();
		final Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(AbstractIdentifiable identifiable : RootBusinessLayer.getInstance().getGenericBusiness()
				.use((Class<? extends AbstractIdentifiable>) StudentClassroomSession.class).findByIdentifiers(identifiers))
			studentClassroomSessions.add((StudentClassroomSession) identifiable);
		return studentClassroomSessions;
	}
	
}
