package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.MetricValueCollection;
import org.cyk.ui.web.primefaces.MetricValueCollection.AbstractMetricValueItem;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputTextarea;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditPage extends AbstractCrudOnePage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private List<MetricValueCollection<StudentResultsMetricValueItem,StudentResultsMetricValue>> metricValueCollections = new ArrayList<>();
	
	@Override
	protected void initialisation() {
		super.initialisation();
		Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = SchoolBusinessLayer.getInstance()
				.getClassroomSessionDivisionStudentsMetricCollectionBusiness().findByClassroomSessionDivision(identifiable.getClassroomSessionDivision());
	
		MetricValueCollection<StudentResultsMetricValueItem,StudentResultsMetricValue> metricValueCollection = null;
		for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : classroomSessionDivisionStudentsMetricCollections){
			final MetricCollection metricCollection = classroomSessionDivisionStudentsMetricCollection.getMetricCollection();			
			metricValueCollection = (MetricValueCollection<StudentResultsMetricValueItem, StudentResultsMetricValue>) createMetricValueCollection(metricCollection,StudentResultsMetricValueItem.class, StudentResultsMetricValue.class 
					,new MetricValueCollection.Adapter<StudentResultsMetricValueItem,StudentResultsMetricValue>(){
				private static final long serialVersionUID = -3872058204105902514L;
				@Override
				public Collection<StudentResultsMetricValue> load() { 
					return SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResultsByMetricCollection(identifiable.getResults(),metricCollection);
				}
			});
			
			metricValueCollections.add(metricValueCollection);
		}
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected <TYPE extends AbstractItemCollectionItem<IDENTIFIABLE>, IDENTIFIABLE extends AbstractIdentifiable> ItemCollection<TYPE, IDENTIFIABLE> instanciateItemCollection(
			String identifier, Class<TYPE> aClass, Class<IDENTIFIABLE> identifiableClass) {
		return new MetricValueCollection(identifier, aClass, identifiableClass);
	}
	
	public MetricValueCollection<StudentResultsMetricValueItem,StudentResultsMetricValue> getMetricValueCollection(Integer index){
		if(index < metricValueCollections.size())
			return metricValueCollections.get(index);
		return null;
	}
		
	@Override
	protected void update() {
		List<StudentResultsMetricValue> studentResultsMetricValues = new ArrayList<>();
		for(MetricValueCollection<StudentResultsMetricValueItem,StudentResultsMetricValue> metricValueCollection : metricValueCollections)
			studentResultsMetricValues.addAll(metricValueCollection.getIdentifiables());
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().update(identifiable, studentResultsMetricValues);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputNumber @NotNull private BigDecimal numberOfTimeAbsent;
		@Input @InputTextarea @NotNull private String appreciation;
		@Input @InputBooleanButton /*@InputChoice @InputOneChoice @InputOneRadio*/ @NotNull private Boolean conferenceRequested;
		
		@Override
		public void read() {
			super.read();
			if(identifiable.getResults().getLectureAttendance().getAttendedDuration()!=null)
				numberOfTimeAbsent = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSessionDivision().getClassroomSession(),identifiable.getResults()
						.getLectureAttendance().getMissedDuration());
			
			appreciation = identifiable.getResults().getAppreciation();
			conferenceRequested = identifiable.getResults().getConferenceRequested();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.getResults().setAppreciation(appreciation);
			identifiable.getResults().setConferenceRequested(conferenceRequested);
			if(numberOfTimeAbsent==null){
				
			}else{
				SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().setNumberOfTimesAbsent(identifiable, numberOfTimeAbsent);
			}
			
		}
	}
	
	@Getter @Setter
	public static class StudentResultsMetricValueItem extends AbstractMetricValueItem<StudentResultsMetricValue> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		
	}

}
