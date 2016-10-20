package org.cyk.system.school.ui.web.primefaces.page;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.model.party.AbstractActorEditFormModel;
import org.cyk.ui.web.primefaces.page.party.AbstractActorEditPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class StudentEditPage extends AbstractActorEditPage.AbstractDefault.Default<Student> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractActorEditFormModel.AbstractDefault.Default<Student> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private LevelTimeDivision admissionLevelTimeDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		
		@Override
		public void read() {
			super.read();
			if(identifiable.getStudentClassroomSession()!=null)
				classroomSession = identifiable.getStudentClassroomSession().getClassroomSession();
		}
		
		@Override
		public void write() {
			super.write();
			if(identifiable.getStudentClassroomSession()!=null)
				identifiable.getStudentClassroomSession().setClassroomSession(classroomSession);
		}
		
		public static final String FIELD_ADMISSION_LEVEL_TIME_DIVISION = "admissionLevelTimeDivision";
		public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	}

}
