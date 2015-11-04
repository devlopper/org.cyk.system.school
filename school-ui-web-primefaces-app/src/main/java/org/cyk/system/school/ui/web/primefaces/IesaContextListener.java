package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionConsultPage;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionConsultPage.AbstractSubjectDetails;
import org.cyk.ui.api.data.collector.form.FormConfiguration.Type;
import org.cyk.ui.api.model.party.DefaultActorEditFormModel;
import org.cyk.ui.api.model.party.DefaultActorReadFormModel;
import org.cyk.ui.api.model.party.DefaultPersonEditFormModel;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormManyPageListener;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormOnePageListener;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

@WebListener
public class IesaContextListener extends AbstractSchoolContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		StudentClassroomSessionDivisionConsultPage.SUBJECT_DETAILS_CLASS_NAME = SubjectDetails.class.getName();
		StudentClassroomSessionDivisionConsultPage.LOAD_EVALUATIONS = Boolean.TRUE;
	}
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormOnePageListener(Class<ACTOR> actorClass,BusinessEntityFormOnePageListener<?> listener) {
		super.registerBusinessEntityFormOnePageListener(actorClass, listener);
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_TITLE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_SURNAME
					,DefaultPersonEditFormModel.FIELD_BIRTH_DATE,DefaultPersonEditFormModel.FIELD_BIRTH_LOCATION
					,DefaultPersonEditFormModel.FIELD_SEX,DefaultPersonEditFormModel.FIELD_IMAGE);
		}
	}
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormManyPageListener(Class<ACTOR> actorClass,BusinessEntityFormManyPageListener<?> listener) {
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}
		super.registerBusinessEntityFormManyPageListener(actorClass, listener);
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		//IdentifiableConfiguration identifiableConfiguration = new IdentifiableConfiguration(StudentClassroomSessionDivision.class, editOneFormModelClass, readOneFormModelClass);
		//uiManager.registerConfiguration(configuration);
	}
	
	/**/
	
	public static class SubjectDetails extends AbstractSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FILED_SUBJECT) private String test1;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST1) private String test2;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST2) private String exam;
		public SubjectDetails(StudentSubject studentSubject) {
			super(studentSubject);
			for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubject.getDetails()){
				if(studentSubjectEvaluation.getStudentSubject().equals(studentSubject)){
					if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Test1"))
						test1 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Test2"))
						test2 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Exam"))
						exam = numberBusiness.format(studentSubjectEvaluation.getValue());
				}
					
			}
		}
		public static final String FIELD_TEST1 = "test1";
		public static final String FIELD_TEST2 = "test2";
		public static final String FIELD_EXAM = "exam";
	}
	
}
