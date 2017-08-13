package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private AcademicSession academicSession;
		@Input @InputChoice @InputOneChoice @InputOneCombo private LevelTimeDivision levelTimeDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSessionSuffix suffix;
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Teacher coordinator;
		
		@IncludeInputs private CommonNodeInformationsFormModel nodeInformations;
		 
		//TODO should not be here but in each page only
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Subject oneSubjectSelected;
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Student oneStudentSelected;
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Long oneOrderNumberSelected;
		
		@Override
		public void read() {
			super.read();
			
		}
		
		@Override
		public void write() {
			super.write();
			
		}
		
		/**/
		
		public static final String FIELD_ACADEMIC_SESSION = "academicSession";
		public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
		public static final String FIELD_SUFFIX = "suffix";
		public static final String FIELD_COORDINATOR = "coordinator";
		public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
		public static final String FIELD_ONE_SUBJECT_SELECTED = "oneSubjectSelected";
		public static final String FIELD_ONE_STUDENT_SELECTED = "oneStudentSelected";
		public static final String FIELD_ONE_ORDER_NUMBER_SELECTED = "oneOrderNumberSelected";
		
	}
	
}
