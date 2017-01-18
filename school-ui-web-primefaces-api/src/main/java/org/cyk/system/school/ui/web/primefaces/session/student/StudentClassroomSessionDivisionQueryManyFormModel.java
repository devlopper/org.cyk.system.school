package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.session.AbstractStudentClassroomSessionDivisionQueryManyFormModel;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyCheck;
import org.cyk.utility.common.annotation.user.interfaces.InputManyChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.cdi.AbstractBean;
import org.omnifaces.util.Faces;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryManyFormModel.FIELD_IDENTIFIABLES,type=ClassroomSession.class)})
public class StudentClassroomSessionDivisionQueryManyFormModel extends AbstractStudentClassroomSessionDivisionQueryManyFormModel<StudentClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	
	
	/**/
	
	public static final String FIELD_PERSON_RELATIONSHIP_TYPES = "personRelationshipTypes";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractStudentClassroomSessionDivisionSelectManyPageAdapter<StudentClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(StudentClassroomSessionDivision.class);
		}
		
		@Override
		public void initialisationEnded(AbstractBean bean) {
			super.initialisationEnded(bean);
		}
				
		@Override
		public Collection<StudentClassroomSessionDivision> getIdentifiables(AbstractSelectManyPage<?> page) {
			AcademicSession academicSession = inject(AcademicSessionBusiness.class).findCurrent(null);
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				return inject(StudentClassroomSessionDivisionBusiness.class).findByAcademicSessionByClassroomSessionDivisionOrderNumber(
						academicSession,academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
			}else
				return inject(StudentClassroomSessionDivisionBusiness.class).findByAcademicSessionByClassroomSessionDivisionOrderNumber(
						academicSession,academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
		}
		
		@Override
		public void serve(AbstractSelectManyPage<?> selectManyPage, Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				@SuppressWarnings({ "unchecked", "rawtypes" })
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = ((AbstractQueryManyFormModel)data).getIdentifiables();
				WebNavigationManager.getInstance().redirectToDynamicProcessManyPage(StudentClassroomSessionDivision.class, studentClassroomSessionDivisions
						, actionIdentifier,new Object[]{ inject(UIManager.class).businessEntityInfos(PersonRelationshipType.class).getIdentifier()
								,inject(WebManager.class).encodeIdentifiablesAsRequestParameterValue(((StudentClassroomSessionDivisionQueryManyFormModel)data)
										.getPersonRelationshipTypes())
						,UniformResourceLocatorParameter.ENCODED,inject(UIManager.class).businessEntityInfos(PersonRelationshipType.class).getIdentifier()});
				//EditManyPage(SchoolWebManager.getInstance().getOutcomeEditStudentClassroomSessionDivisionEvaluationAverage()
						//,StudentClassroomSessionDivision.class,studentClassroomSessionDivisions);
			}else
				super.serve(selectManyPage, data, actionIdentifier);
		}
	}
	
	@Getter @Setter
	public static class ProcessPageAdapter extends AbstractProcessManyPage.Listener.Adapter.Default<StudentClassroomSessionDivision,Long> implements Serializable {

		private static final long serialVersionUID = -8606970206843948983L;

		public ProcessPageAdapter() {
			super(StudentClassroomSessionDivision.class);
		}
		
		@Override
		protected void initialiseProcessOnInitialisationEnded(final AbstractProcessManyPage<?> page) {
			super.initialiseProcessOnInitialisationEnded(page);
			SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
			page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
			if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionSendStudentClassroomSessionDivisionReportFiles()}, page.getActionIdentifier())){
				Collection<PersonRelationshipType> personRelationshipTypes = inject(WebManager.class).decodeIdentifiablesRequestParameter(PersonRelationshipType.class
						, inject(UIManager.class).businessEntityInfos(PersonRelationshipType.class).getIdentifier(), Faces.getRequest());
				((SendMessageForm)page.getForm().getData()).setPersonRelationshipTypes(personRelationshipTypes.toString());
				
				System.out.println(personRelationshipTypes);
				
				Collection<Student> students = new ArrayList<>();
				for(StudentClassroomSessionDivision studentClassroomSessionDivision : commonUtils.castCollection(page.getElements(), StudentClassroomSessionDivision.class))
					students.add(studentClassroomSessionDivision.getStudent());
				page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
				Collection<Person> persons = inject(PersonBusiness.class).get(students);
				Collection<Person> parents = inject(PersonBusiness.class).findByPersonRelationshipPerson2ByPersonRelationshipTypes(persons, personRelationshipTypes);
				((SendMessageForm)page.getForm().getData()).setReceivers(parents.toString());
				
				System.out.println(parents);
				
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){

					private static final long serialVersionUID = 1L;

					@Override
					public Boolean build(Object data,Field field) {
						return ArrayUtils.contains(new String[]{ProcessPageAdapter.SendMessageForm.FIELD_PERSON_RELATIONSHIP_TYPES
								,ProcessPageAdapter.SendMessageForm.FIELD_RECEIVERS}, field.getName());
					}
					
				});
				
			}
		}
		
		@Override
		protected void initialiseProcessOnAfterInitialisationEnded(final AbstractProcessManyPage<?> page) {
			super.initialiseProcessOnAfterInitialisationEnded(page);
			
		}
		
		@Override
		public void serve(AbstractProcessManyPage<?> page,Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				inject(StudentClassroomSessionDivisionBusiness.class).sendReportFileToEmail(commonUtils.castCollection(page.getElements(), StudentClassroomSessionDivision.class));
			}
		}
		
		@Override
		public Class<?> getFormDataClass(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier))
				return SendMessageForm.class;
			return Form.class;
		}
		
		@Override
		public Boolean getShowForm(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return ArrayUtils.contains(new String[]{SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles()}, actionIdentifier);
		}
		
		@Getter @Setter
		public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			@Input @InputBooleanButton private Boolean updateEvaluationResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateAttendanceResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateRankResults=Boolean.TRUE;
			
			@Input @InputNumber private Integer classroomSessionDivisionMinCount;
			@Input @InputNumber private Integer classroomSessionDivisionMaxCount;
			@Input @InputChoice @InputManyChoice @InputManyCheck private List<Integer> classroomSessionDivisionIndexesRequired;
			
			@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private File backgroundImageFile;
			@Input @InputBooleanButton private Boolean draft=Boolean.FALSE;
			
			public static final String FIELD_UPDATE_EVALUATION_RESULTS = "updateEvaluationResults";
			public static final String FIELD_UPDATE_ATTENDANCE_RESULTS = "updateAttendanceResults";
			public static final String FIELD_UPDATE_RANK_RESULTS = "updateRankResults";
			
			public static final String FIELD_CLASSROOMSESSIONDIVISION_MIN_COUNT = "classroomSessionDivisionMinCount";
			public static final String FIELD_CLASSROOMSESSIONDIVISION_MAX_COUNT = "classroomSessionDivisionMaxCount";
			public static final String FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED = "classroomSessionDivisionIndexesRequired";
			
			public static final String FIELD_DRAFT = "draft";
			public static final String FIELD_BACKGROUND_IMAGE_FILE = "backgroundImageFile";
		}
		
		@Getter @Setter
		public static class SendMessageForm extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			
			@Input(readOnly=true,disabled=true) @InputText private String personRelationshipTypes;
			@Input(readOnly=true,disabled=true) @InputText private String receivers;
			
			public static final String FIELD_PERSON_RELATIONSHIP_TYPES = "personRelationshipTypes";
			public static final String FIELD_RECEIVERS = "receivers";
			
		}
	}
}