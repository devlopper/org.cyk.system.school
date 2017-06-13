package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.ui.web.primefaces.session.AbstractStudentClassroomSessionConsultManyRankPage;
import org.cyk.ui.web.api.WebManager;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultManyRankPage extends AbstractStudentClassroomSessionConsultManyRankPage implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void consultInitialisation() {
		// TODO Auto-generated method stub
		//super.consultInitialisation();
	}
	
	@Override
	protected Collection<StudentClassroomSession> getStudentClassroomSessions() {
		Collection<Long> identifiers = WebManager.getInstance().decodeIdentifiersRequestParameter();
		final Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(AbstractIdentifiable identifiable : inject(GenericBusiness.class)
				.use((Class<? extends AbstractIdentifiable>) StudentClassroomSession.class).findByIdentifiers(identifiers))
			studentClassroomSessions.add((StudentClassroomSession) identifiable);
		return studentClassroomSessions;
	}
	
	@Override
	protected Class<StudentClassroomSession> getClassParameter() {
		return StudentClassroomSession.class;
	}
}
