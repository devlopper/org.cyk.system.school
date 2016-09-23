package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class SubjectClassroomSessionEditPage extends AbstractCrudOnePage<SubjectClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		identifiable.setCascadeOperationToChildren(Boolean.TRUE);
	}
	
	@Override
	protected SubjectClassroomSession instanciateIdentifiable() {
		SubjectClassroomSession subjectClassroomSession = super.instanciateIdentifiable();
		subjectClassroomSession.setClassroomSession(webManager.getIdentifiableFromRequestParameter(ClassroomSession.class, Boolean.TRUE));
		return subjectClassroomSession;
	}
	
	public static class Form extends AbstractFormModel<SubjectClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(readOnly=true,disabled=true) @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Subject subject;
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Teacher teacher;
		@Input @InputNumber private BigDecimal coefficient = BigDecimal.ONE;
		
		public static final String FIELD_SUBJECT = "subject";
		public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
		public static final String FIELD_TEACHER = "teacher";
		public static final String FIELD_COEFFICIENT = "coefficient";
	}
	
}
