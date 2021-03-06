package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionUpdateStudentResultsPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<Result,StudentClassroomSessionDivision,StudentClassroomSession> resultCollection;
	private Integer appreciationMaxLenght = 255;
	private BigDecimal maximumMissedDuration;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		resultCollection = createItemCollection(Result.class, StudentClassroomSessionDivision.class,null,new ItemCollectionWebAdapter<Result,StudentClassroomSessionDivision,StudentClassroomSession>(null,crud){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public Collection<StudentClassroomSessionDivision> load() {
				return inject(StudentClassroomSessionDivisionBusiness.class).findByClassroomSessionDivision(identifiable);
			}
			@Override
			public void instanciated(AbstractItemCollection<Result, StudentClassroomSessionDivision,StudentClassroomSession,SelectItem> itemCollection,Result result) {
				super.instanciated(itemCollection, result);
				result.setRegistrationCode(result.getIdentifiable().getStudent().getCode());
				result.setNames(result.getIdentifiable().getStudent().getPerson().getNames());
				result.setAppreciation(result.getIdentifiable().getResults().getAppreciation());
				//result.setConferenceRequested(result.getIdentifiable().getResults().getConferenceRequested());
				/*
				if(result.getIdentifiable().getResults().getLectureAttendance().getAttendedDuration()!=null)
					result.setNumberOfTimeAbsent(inject(ClassroomSessionBusiness.class)
					.convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSession(),result.getIdentifiable().getResults()
							.getLectureAttendance().getMissedDuration()));
							*/
			}	
			@Override
			public void write(Result item) {
				super.write(item);
				item.getIdentifiable().getResults().setAppreciation(item.getAppreciation());
				//item.getIdentifiable().getResults().setConferenceRequested(item.getConferenceRequested());
				
				if(item.getNumberOfTimeAbsent()==null){
					
				}else{
					inject(StudentClassroomSessionDivisionBusiness.class).setNumberOfTimesAbsent(item.getIdentifiable(), item.getNumberOfTimeAbsent());
				}
			}
		});
		resultCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
		resultCollection.getApplicableValueQuestion().setRendered(Boolean.FALSE);
		resultCollection.getAddCommandable().setRendered(Boolean.FALSE);
		
		maximumMissedDuration = inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSession(),identifiable.getExistencePeriod().getNumberOfMillisecond().get());
	}
	
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.UPDATE;
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(ClassroomSessionDivision.class);
	}
	
	@Override
	protected void update() {
		inject(StudentClassroomSessionDivisionBusiness.class).update(resultCollection.getIdentifiables());
	}
	
	@Override
	protected Boolean consultOnSuccess() {
		return Boolean.TRUE;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = instanciateCommandableBuilder().setLabel(formatUsingBusiness(identifiable)).create();
		
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSession(), null));
		
		return Arrays.asList(contextualMenu);
	}
		
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		

	}
	
	@Getter @Setter
	public static class Result extends AbstractItemCollectionItem<StudentClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String registrationCode;
		private String names;
		private String appreciation;
		//private Boolean conferenceRequested;
		private BigDecimal numberOfTimeAbsent;
	}

}
