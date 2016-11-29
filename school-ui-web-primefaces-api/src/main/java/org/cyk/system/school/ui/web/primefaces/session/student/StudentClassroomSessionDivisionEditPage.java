package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.FormatterBusiness;
import org.cyk.system.root.business.api.mathematics.MetricBusiness;
import org.cyk.system.root.business.api.mathematics.MetricCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.MetricCollectionTypeBusiness;
import org.cyk.system.root.business.api.mathematics.MetricValueBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.primefaces.AbstractMetricValueCollection;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.MetricValueCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice.ChoiceSet;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneRadio;
import org.cyk.utility.common.annotation.user.interfaces.InputTextarea;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditPage extends AbstractCrudOnePage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private List<MetricValueCollection> metricValueCollections = new ArrayList<>();
	
	@Override
	protected void initialisation() {
		super.initialisation();
		Collection<MetricCollection> metricCollections = inject(MetricCollectionBusiness.class).findByTypesByIdentifiable(inject(MetricCollectionTypeBusiness
				.class).find(Arrays.asList(SchoolConstant.Code.MetricCollectionType.STUDENT_BEHAVIOUR,SchoolConstant.Code.MetricCollectionType.STUDENT_ATTENDANCE))
				, identifiable.getClassroomSessionDivision());
		
		MetricValueCollection metricValueCollection = null;
		for(MetricCollection metricCollection : metricCollections){
			final MetricCollection lMetricCollection = metricCollection;			
			metricValueCollection = (MetricValueCollection) createMetricValueCollection(metricCollection,new MetricValueCollection.Adapter(){
				private static final long serialVersionUID = -3872058204105902514L;
				@Override
				public Collection<MetricValue> load() { 
					return inject(MetricValueBusiness.class).findByMetricsByIdentifiables(inject(MetricBusiness.class).findByCollection(lMetricCollection)
							, Arrays.asList(identifiable));
				}
			});
			//metricValueCollection.addNullChoice();
			metricValueCollections.add(metricValueCollection);
		}
	}
	
	@SuppressWarnings({ "unchecked" })
	@Override
	protected <TYPE extends AbstractItemCollectionItem<IDENTIFIABLE>, IDENTIFIABLE extends AbstractIdentifiable> ItemCollection<TYPE, IDENTIFIABLE> instanciateItemCollection(
			String identifier, Class<TYPE> aClass, Class<IDENTIFIABLE> identifiableClass) {
		return (ItemCollection<TYPE, IDENTIFIABLE>) new MetricValueCollection(identifier);
	}
	
	public AbstractMetricValueCollection<MetricValueCollection.Item,MetricValue> getMetricValueCollection(Integer index){
		if(index < metricValueCollections.size())
			return metricValueCollections.get(index);
		return null;
	}
	/*	
	@Override
	protected void update() {
		List<MetricValue> metricValues = new ArrayList<>();
		for(MetricValueCollection metricValueCollection : metricValueCollections)
			metricValues.addAll(metricValueCollection.getIdentifiables());
		update(identifiable, metricValues);
	}*/
	
	@Override
	protected Collection<? extends AbstractIdentifiable> getIdentifiables() {
		if(Crud.UPDATE.equals(crud)){
			Collection<AbstractIdentifiable> collection = new ArrayList<>();
			for(MetricValueCollection metricValueCollection : metricValueCollections)
				for(AbstractIdentifiable metricValue : metricValueCollection.getIdentifiables())
					collection.add(metricValue);
			return collection;
		}
		return super.getIdentifiables();
	}
	
	/*
	@Override
	protected Class<?> __formModelClass__() {
		return One.class;
	}*/
	
	public static class One extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		//@Input @InputNumber private BigDecimal evaluationAverage;
		@Input @InputTextarea @NotNull private String appreciation;
		@Input /*@InputBooleanButton*/ @InputChoice(set=ChoiceSet.YES_NO) @InputOneChoice @InputOneRadio @NotNull private Boolean conferenceRequested;
		
		@Override
		public void read() {
			super.read();
			//evaluationAverage = identifiable.getResults().getEvaluationSort().getAverage().getValue();
			/*
			if(identifiable.getResults().getLectureAttendance().getAttendedDuration()!=null)
				numberOfTimeAbsent = inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSessionDivision().getClassroomSession(),identifiable.getResults()
						.getLectureAttendance().getMissedDuration());
			*/
			appreciation = identifiable.getResults().getAppreciation();
			conferenceRequested = identifiable.getResults().getConferenceRequested();
		}
		
		@Override
		public void write() {
			super.write();
			//identifiable.getResults().getEvaluationSort().getAverage().setValue(evaluationAverage);
			identifiable.getResults().setAppreciation(appreciation);
			identifiable.getResults().setConferenceRequested(conferenceRequested);
			/*if(numberOfTimeAbsent==null){
				
			}else{
				inject(StudentClassroomSessionDivisionBusiness.class).setNumberOfTimesAbsent(identifiable, numberOfTimeAbsent);
			}*/
			
		}
	}
	
	@Getter @Setter
	public static class Many extends AbstractItemCollectionItem<StudentClassroomSessionDivision> implements Serializable{
		
		private static final long serialVersionUID = -829786138986362643L;

		@Input @InputNumber private String classroomSession;
		@Input @InputNumber private BigDecimal evaluationAverage;
		
		/**/
		
		public static class ItemCollectionAdapter extends org.cyk.ui.web.primefaces.page.crud.AbstractEditManyPage.ItemCollectionAdapter<StudentClassroomSessionDivisionEditPage.Many, StudentClassroomSessionDivision> {
			private static final long serialVersionUID = -5381415970572336750L;
				
			public ItemCollectionAdapter() {
				super(UIManager.getInstance().businessEntityInfos(StudentClassroomSessionDivision.class));
			}
			
			@Override
			public void instanciated(AbstractItemCollection<StudentClassroomSessionDivisionEditPage.Many, StudentClassroomSessionDivision, SelectItem> itemCollection
					,StudentClassroomSessionDivisionEditPage.Many item) {
				super.instanciated(itemCollection, item);
				item.setLabel(inject(FormatterBusiness.class).format(item.getIdentifiable().getClassroomSessionDivision().getClassroomSession())
						+Constant.CHARACTER_SLASH+item.getLabel());
				item.setClassroomSession(inject(FormatterBusiness.class).format(item.getIdentifiable().getClassroomSessionDivision().getClassroomSession()));
				item.setEvaluationAverage(item.getIdentifiable().getResults().getEvaluationSort().getAverage().getValue());
			}
			
			@Override
			public void write(StudentClassroomSessionDivisionEditPage.Many item) {
				super.write(item);
				item.getIdentifiable().getResults().getEvaluationSort().getAverage().setValue(item.getEvaluationAverage());
			}
			
		}
	}
}
