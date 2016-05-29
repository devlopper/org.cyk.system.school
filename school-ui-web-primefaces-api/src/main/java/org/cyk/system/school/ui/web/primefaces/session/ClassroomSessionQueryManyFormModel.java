package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness.ServiceCallArguments;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.PrimefacesManager;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.computation.ExecutionProgress;

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
				return SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
			}else if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				return SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
			}else
				return SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
		}
		
		@Override
		public void serve(AbstractSelectManyPage<?> selectManyPage, Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage().equals(actionIdentifier)){
				@SuppressWarnings({ "unchecked", "rawtypes" })
				Collection<ClassroomSession> classroomSessions = ((AbstractQueryManyFormModel)data).getIdentifiables();
				
				Collection<ClassroomSessionDivision> classroomSessionDivisions = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSessionsByIndex(classroomSessions,
						SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisions(classroomSessionDivisions);
				
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
					@Override
					public Boolean build(Field field) {
						if(SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(page.getActionIdentifier()))
							return field.getName().equals(Form.FIELD_UPDATE_RANK_RESULTS);
						return super.build(field);
					}
				});
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults()}, page.getActionIdentifier())){
				page.getForm().getSubmitCommandable().getCommand().setShowExecutionProgress(Boolean.TRUE);
				page.setExecutionProgress(new ExecutionProgress("Compute Student Classroom Session "+page.getActionIdentifier(),null));
				page.getForm().getSubmitCommandable().getCommand().setExecutionProgress(page.getExecutionProgress());
				PrimefacesManager.getInstance().configureProgressBar(page.getForm().getSubmitCommandable());
				
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){
					@Override
					public Boolean build(Field field) {
						if(SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(page.getActionIdentifier()))
							return field.getName().equals(Form.FIELD_UPDATE_RANK_RESULTS);
						return super.build(field);
					}
				});
			}
		}
		
		@Override
		public void serve(AbstractProcessManyPage<?> page,Object data, String actionIdentifier) {
			SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
			Collection<ClassroomSession> classroomSessions = new ArrayList<>();
			for(Object object : page.getElements())
				classroomSessions.add((ClassroomSession) object);
			
			if(SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = schoolBusinessLayer.getClassroomSessionDivisionBusiness().findByClassroomSessions(classroomSessions);
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisions(classroomSessionDivisions);
				schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().findReportFiles(studentClassroomSessionDivisions);
				WebNavigationManager.getInstance().redirectToFileConsultManyPage(schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().findReportFiles(studentClassroomSessionDivisions), FileExtension.PDF);
			}else if(schoolBusinessLayer.getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = schoolBusinessLayer.getClassroomSessionDivisionBusiness()
						.findByClassroomSessionsByIndex(classroomSessions,schoolBusinessLayer.getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				ServiceCallArguments callArguments = new ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				Form form = (Form) data;
				RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
		        rankOptions.setType(RankType.EXAEQUO); 
		        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
				schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().buildReport(classroomSessionDivisions,form.getUpdateEvaluationResults(),form.getUpdateAttendanceResults()
						,form.getUpdateRankResults(),rankOptions,callArguments);
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionAttendanceResults()
					,schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionEvaluationResults(),schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionRankResults()}
				, actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = schoolBusinessLayer.getClassroomSessionDivisionBusiness()
						.findByClassroomSessionsByIndex(classroomSessions,schoolBusinessLayer.getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				ServiceCallArguments callArguments = new ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionAttendanceResults().equals(actionIdentifier))
					schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().updateAttendance(classroomSessionDivisions, callArguments);
				else if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(actionIdentifier)){
					Form form = (Form) data;
					RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
			        rankOptions.setType(RankType.EXAEQUO); 
			        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
					schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().updateResults(classroomSessionDivisions,Boolean.TRUE,form.getUpdateRankResults()
							,rankOptions,Boolean.FALSE,callArguments);
				}else if(schoolBusinessLayer.getActionComputeStudentClassroomSessionDivisionRankResults().equals(actionIdentifier)){
					RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
			        rankOptions.setType(RankType.EXAEQUO); 
			        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
					schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().updateRank(classroomSessionDivisions, rankOptions,callArguments);
				}
			}else if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults()}, actionIdentifier)){
				StudentClassroomSessionBusiness.ServiceCallArguments callArguments = new StudentClassroomSessionBusiness.ServiceCallArguments();
				callArguments.setExecutionProgress(page.getExecutionProgress());
				if(schoolBusinessLayer.getActionComputeStudentClassroomSessionEvaluationResults().equals(actionIdentifier)){
					Form form = (Form) data;
					RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
			        rankOptions.setType(RankType.EXAEQUO); 
			        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
					schoolBusinessLayer.getStudentClassroomSessionBusiness().updateResults(classroomSessions,Boolean.TRUE,form.getUpdateRankResults()
							,rankOptions,Boolean.FALSE,callArguments);
				}
			}
		}
		
		@Override
		public Class<?> getFormDataClass(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return Form.class;
		}
		
		@Override
		public Boolean getShowForm(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)
					|| SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults().equals(actionIdentifier);
		}
		
		@Getter @Setter
		public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			@Input @InputBooleanButton private Boolean updateEvaluationResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateAttendanceResults=Boolean.TRUE;
			@Input @InputBooleanButton private Boolean updateRankResults=Boolean.TRUE;
			
			public static final String FIELD_UPDATE_EVALUATION_RESULTS = "updateEvaluationResults";
			public static final String FIELD_UPDATE_ATTENDANCE_RESULTS = "updateAttendanceResults";
			public static final String FIELD_UPDATE_RANK_RESULTS = "updateRankResults";
		}
		
	}
}