package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.faces.model.SelectItem;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.impl.validation.ExceptionUtils;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.file.FileRepresentationTypeDao;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness.ServiceCallArguments;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.data.collector.form.ControlSet;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.PrimefacesManager;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyCheck;
import org.cyk.utility.common.annotation.user.interfaces.InputManyChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.computation.ExecutionProgress;
import org.primefaces.extensions.model.dynaform.DynaFormControl;
import org.primefaces.extensions.model.dynaform.DynaFormLabel;
import org.primefaces.extensions.model.dynaform.DynaFormModel;
import org.primefaces.extensions.model.dynaform.DynaFormRow;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryManyFormModel.FIELD_IDENTIFIABLES,type=ClassroomSession.class)})
public class ClassroomSessionQueryManyFormModel extends AbstractClassroomSessionQueryManyFormModel<ClassroomSession> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionSelectManyPageAdapter<ClassroomSession> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSession.class);
		}
				
		@Override
		public Collection<ClassroomSession> getIdentifiables(AbstractSelectManyPage<?> page) {
			/*if(SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage().equals(page.getActionIdentifier())){
				WebNavigationManager.getInstance().redirectToEditManyPage(SchoolWebManager.getInstance().getOutcomeEditStudentClassroomSessionDivisionEvaluationAverage(),StudentClassroomSessionDivision.class,((AbstractQueryManyFormModel)data).getIdentifiables());
			}else */if(SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				return inject(ClassroomSessionBusiness.class).findAll();
			}else if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				return inject(ClassroomSessionBusiness.class).findAll();
			}else
				return inject(ClassroomSessionBusiness.class).findAll();
		}
		
		@Override
		public void serve(AbstractSelectManyPage<?> selectManyPage, Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage().equals(actionIdentifier)){
				@SuppressWarnings({ "unchecked", "rawtypes" })
				Collection<ClassroomSession> classroomSessions = ((AbstractQueryManyFormModel)data).getIdentifiables();
				
				Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionsByOrderNumber(classroomSessions,
						inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSessionDivisions(classroomSessionDivisions);
				
				WebNavigationManager.getInstance().redirectToEditManyPage(SchoolWebManager.getInstance().getOutcomeEditStudentClassroomSessionDivisionEvaluationAverage(),StudentClassroomSessionDivision.class
						,studentClassroomSessionDivisions);
			}else
				super.serve(selectManyPage, data, actionIdentifier);
		}
	}
	
	@Getter @Setter
	public static class ProcessPageAdapter extends AbstractProcessManyPage.Listener.Adapter.Default<ClassroomSession,Long> implements Serializable {

		private static final long serialVersionUID = -8606970206843948983L;

		public ProcessPageAdapter() {
			super(ClassroomSession.class);
		}
		
		private Collection<ClassroomSession> getClassroomSessions(AbstractProcessManyPage<?> page){
			Collection<ClassroomSession> classroomSessions = new ArrayList<>();
			for(Object object : page.getElements()){
				classroomSessions.add((ClassroomSession) object);
			}
			return classroomSessions;
		}
		
		private Set<TimeDivisionType> getTimeDivisionTypes(AbstractProcessManyPage<?> page){
			Set<TimeDivisionType> timeDivisionTypes = new LinkedHashSet<>();
			for(Object object : page.getElements()){
				CommonNodeInformations nodeInformations = inject(ClassroomSessionBusiness.class).findCommonNodeInformations((ClassroomSession) object);
				timeDivisionTypes.add(nodeInformations.getClassroomSessionTimeDivisionType());
			}
			return timeDivisionTypes;
		}
		
		@Override
		protected void initialiseProcessOnInitialisationEnded(final AbstractProcessManyPage<?> page) {
			super.initialiseProcessOnInitialisationEnded(page);

			SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
			page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
			if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				page.getForm().getSubmitCommandable().getCommand().setShowExecutionProgress(Boolean.TRUE);
				page.setExecutionProgress(new ExecutionProgress("Build Student Classroom Session Division Report",null));
				page.getForm().getSubmitCommandable().getCommand().setExecutionProgress(page.getExecutionProgress());
				PrimefacesManager.getInstance().configureProgressBar(page.getForm().getSubmitCommandable());
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionAttendanceResults()
					,schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionEvaluationResults(),schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionRankResults()}
				, page.getActionIdentifier())){
				page.getForm().getSubmitCommandable().getCommand().setShowExecutionProgress(Boolean.TRUE);
				page.setExecutionProgress(new ExecutionProgress("Compute Student Classroom Session Division "+page.getActionIdentifier(),null));
				page.getForm().getSubmitCommandable().getCommand().setExecutionProgress(page.getExecutionProgress());
				PrimefacesManager.getInstance().configureProgressBar(page.getForm().getSubmitCommandable());
				
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){

					private static final long serialVersionUID = 1L;

					@Override
					public Boolean build(Object data,Field field) {
						if(SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(page.getActionIdentifier()))
							return field.getName().equals(ProcessPageAdapter.Form.FIELD_UPDATE_RANK_RESULTS);
						return super.build(data,field);
					}
				});
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults()}, page.getActionIdentifier())){
				page.getForm().getSubmitCommandable().getCommand().setShowExecutionProgress(Boolean.TRUE);
				page.setExecutionProgress(new ExecutionProgress("Compute Student Classroom Session "+page.getActionIdentifier(),null));
				page.getForm().getSubmitCommandable().getCommand().setExecutionProgress(page.getExecutionProgress());
				PrimefacesManager.getInstance().configureProgressBar(page.getForm().getSubmitCommandable());
				
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){

					private static final long serialVersionUID = 1L;

					@Override
					public Boolean build(Object data,Field field) {
						if(SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(page.getActionIdentifier()))
							return field.getName().equals(ProcessPageAdapter.Form.FIELD_UPDATE_RANK_RESULTS);
						return super.build(data,field);
					}
				});
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionConsultStudentClassroomSessionRanks()}, page.getActionIdentifier())){
				final Set<TimeDivisionType> timeDivisionTypes = getTimeDivisionTypes(page);
				page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.FALSE);
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){

					private static final long serialVersionUID = 1L;

					@Override
					public Boolean build(Object data,Field field) {
						return ArrayUtils.contains(new String[]{ProcessPageAdapter.Form.FIELD_CLASSROOMSESSIONDIVISION_MIN_COUNT
								,ProcessPageAdapter.Form.FIELD_CLASSROOMSESSIONDIVISION_MAX_COUNT
								,ProcessPageAdapter.Form.FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED}, field.getName());
					}
					
					@Override
					public String fiedLabel(ControlSet<Object, DynaFormModel, DynaFormRow, DynaFormLabel, DynaFormControl, SelectItem> controlSet,Object data,Field field) {
						if(field.getName().equals(ProcessPageAdapter.Form.FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED) && timeDivisionTypes.size()==1)
							return timeDivisionTypes.iterator().next().getName();
						return super.fiedLabel(controlSet,data, field);
					}
					
					
				});
			}
		}
		
		@Override
		protected void initialiseProcessOnAfterInitialisationEnded(final AbstractProcessManyPage<?> page) {
			super.initialiseProcessOnAfterInitialisationEnded(page);
			Set<TimeDivisionType> timeDivisionTypes = getTimeDivisionTypes(page);
			Set<Byte> indexes = new LinkedHashSet<>();
			//Set<TimeDivisionType> timeDivisionTypesProcessed = new LinkedHashSet<>();
			Collection<ClassroomSession> classroomSessions = getClassroomSessions(page);
			@SuppressWarnings("unchecked")
			org.cyk.ui.api.data.collector.control.InputChoice<?,?,?,?,?,SelectItem> input = page.getForm().findInputByClassByFieldName(org.cyk.ui.api.data.collector.control.InputChoice.class, Form.FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED);
			if(input!=null)
				for(ClassroomSession classroomSession : classroomSessions){
					CommonNodeInformations nodeInformations = inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession);
					if(nodeInformations.getClassroomSessionDivisionOrderNumberInterval()!=null){
						Long max = inject(IntervalBusiness.class).findLowestGreatestValue(nodeInformations.getClassroomSessionDivisionOrderNumberInterval()).longValue();
						if(max!=null)
							for(byte i = 0; i < max; i++){
								if(indexes.add(i) /*|| timeDivisionTypesProcessed.add(nodeInformations.getClassroomSessionTimeDivisionType())*/)
									input.getList().add(new SelectItem(i, (timeDivisionTypes.size()==1?Constant.EMPTY_STRING
										:nodeInformations.getAttendanceTimeDivisionType().getName()+Constant.CHARACTER_SPACE)+String.valueOf(i+1)));
							}
					}
				}
			
			//page.setChoices(Form.FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED, indexes);
		}
		
		@Override
		public void serve(AbstractProcessManyPage<?> page,Object data, String actionIdentifier) {
			SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
			Collection<ClassroomSession> classroomSessions = getClassroomSessions(page);
			
			if(SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionsByOrderNumber(classroomSessions,
						inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSessionDivisions(classroomSessionDivisions);
				WebNavigationManager.getInstance().redirectToFileConsultManyPage(inject(FileRepresentationTypeDao.class).read(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET)
						,studentClassroomSessionDivisions, FileExtension.PDF);
			}else if(schoolBusinessLayer.getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionBusiness.class)
						.findByClassroomSessionsByOrderNumber(classroomSessions,inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				ServiceCallArguments callArguments = new ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				Form form = (Form) data;
				inject(StudentClassroomSessionDivisionBusiness.class).buildReport(classroomSessionDivisions,form.getUpdateEvaluationResults(),form.getUpdateAttendanceResults()
						,form.getUpdateRankResults(),schoolBusinessLayer.getStudentEvaluationResultsRankOptions(),callArguments);
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionAttendanceResults()
					,schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionEvaluationResults(),schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionRankResults()}
				, actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = inject(ClassroomSessionDivisionBusiness.class)
						.findByClassroomSessionsByOrderNumber(classroomSessions,inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				ServiceCallArguments callArguments = new ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionAttendanceResults().equals(actionIdentifier))
					inject(StudentClassroomSessionDivisionBusiness.class).updateAttendance(classroomSessionDivisions, callArguments);
				else if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(actionIdentifier)){
					Form form = (Form) data;
					inject(StudentClassroomSessionDivisionBusiness.class).updateResults(classroomSessionDivisions,Boolean.TRUE,form.getUpdateRankResults()
							,schoolBusinessLayer.getStudentEvaluationResultsRankOptions(),Boolean.FALSE,callArguments);
				}else if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionRankResults().equals(actionIdentifier)){
					inject(StudentClassroomSessionDivisionBusiness.class).updateRank(classroomSessionDivisions, schoolBusinessLayer.getStudentEvaluationResultsRankOptions(),callArguments);
				}
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults()}, actionIdentifier)){
				StudentClassroomSessionBusiness.ServiceCallArguments callArguments = new StudentClassroomSessionBusiness.ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				if(schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults().equals(actionIdentifier)){
					Form form = (Form) data;
					inject(StudentClassroomSessionBusiness.class).updateResults(classroomSessions,Boolean.TRUE,form.getUpdateRankResults()
							,schoolBusinessLayer.getStudentEvaluationResultsRankOptions(),Boolean.FALSE,callArguments);
				}
			}else if(SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionRanks().equals(actionIdentifier)){
				StudentClassroomSession.SearchCriteria searchCriteria = new StudentClassroomSession.SearchCriteria();
				searchCriteria.getDivisionCount().setLowest(((Form)data).getClassroomSessionDivisionMinCount());
				searchCriteria.getDivisionCount().setHighest(((Form)data).getClassroomSessionDivisionMaxCount());
				searchCriteria.getDivisionIndexesRequired().addAll(((Form)data).getClassroomSessionDivisionIndexesRequired());
				
				Collection<StudentClassroomSession> studentClassroomSessions = inject(StudentClassroomSessionBusiness.class).findByCriteria(searchCriteria);
				if(studentClassroomSessions.isEmpty())
					ExceptionUtils.getInstance().exception("noresultsfound");
				WebNavigationManager.getInstance().redirectToDynamicProcessManyPage(SchoolWebManager.getInstance().getOutcomeStudentClassroomSessionConsultManyRank(),StudentClassroomSession.class
						,studentClassroomSessions,actionIdentifier);
			}
		}
		
		@Override
		public Class<?> getFormDataClass(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return Form.class;
		}
		
		@Override
		public Boolean getShowForm(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return ArrayUtils.contains(new String[]{SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles()
					,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults()
					,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionRanks()}, actionIdentifier);
		}
		
		@Getter @Setter
		public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			@Input @InputBooleanButton private Boolean updateEvaluationResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateAttendanceResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateRankResults=Boolean.TRUE;
			
			@Input @InputNumber private Integer classroomSessionDivisionMinCount;
			@Input @InputNumber private Integer classroomSessionDivisionMaxCount;
			@Input @InputChoice @InputManyChoice @InputManyCheck private List<Integer> classroomSessionDivisionIndexesRequired;
			
			public static final String FIELD_UPDATE_EVALUATION_RESULTS = "updateEvaluationResults";
			public static final String FIELD_UPDATE_ATTENDANCE_RESULTS = "updateAttendanceResults";
			public static final String FIELD_UPDATE_RANK_RESULTS = "updateRankResults";
			
			public static final String FIELD_CLASSROOMSESSIONDIVISION_MIN_COUNT = "classroomSessionDivisionMinCount";
			public static final String FIELD_CLASSROOMSESSIONDIVISION_MAX_COUNT = "classroomSessionDivisionMaxCount";
			public static final String FIELD_CLASSROOMSESSIONDIVISION_INDEXES_REQUIRED = "classroomSessionDivisionIndexesRequired";
		}
	}
}