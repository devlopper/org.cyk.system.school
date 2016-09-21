package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectEditPage extends AbstractCrudOnePage<ClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractFormModel<ClassroomSessionDivisionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(readOnly=true,disabled=true) @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSessionDivision classroomSessionDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private Subject subject;
		@Input @InputChoice @InputOneChoice @InputOneCombo private Teacher teacher;
		@Input @InputNumber private BigDecimal coefficient;
		@Input @InputNumber private BigDecimal duration;
		
		@Override
		public void read() {
			super.read();
			if(identifiable.getClassroomSessionDivision()!=null)
				classroomSession = identifiable.getClassroomSessionDivision().getClassroomSession();
		}
		
		/**/
		
		public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
		public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
		public static final String FIELD_SUBJECT = "subject";
		public static final String FIELD_TEACHER = "teacher";
		public static final String FIELD_COEFFICIENT = "coefficient";
		public static final String FIELD_DURATION = "duration";
		
	}

}
