package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private AcademicSession academicSession;
		@Input @InputChoice @InputOneChoice @InputOneCombo private LevelTimeDivision levelTimeDivision;
		@Input @InputText private String suffix;
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Teacher coordinator;
		@Input @InputNumber private Integer numberOfStudent;
		
		@Override
		public void read() {
			super.read();
			numberOfStudent = identifiable.getResults().getNumberOfStudent();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.getResults().setNumberOfStudent(numberOfStudent);
		}
		
		/**/
		
		public static final String FIELD_ACADEMIC_SESSION = "academicSession";
		public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
		public static final String FIELD_SUFFIX = "suffix";
		public static final String FIELD_NUMBER_OF_STUDENT = "numberOfStudent";
		public static final String FIELD_COORDINATOR = "coordinator";
		
	}

}
