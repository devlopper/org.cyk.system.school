package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.command.UICommand;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.api.model.ItemCollectionListener.ItemCollectionAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditPage extends AbstractCrudOnePage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<MetricValueItem,StudentResultsMetricValue> metricValueCollection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		//contentTitle = languageBusiness.findClassLabelText(AcademicSession.class)+" : "+identifiable.getAcademicSession().getUiString()
		//		+" - "+SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable);
		
		metricValueCollection = createItemCollection(form, "qwerty", MetricValueItem.class, StudentResultsMetricValue.class, 
				SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(identifiable.getResults()),new ItemCollectionAdapter<MetricValueItem,StudentResultsMetricValue>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<MetricValueItem, StudentResultsMetricValue> itemCollection,MetricValueItem item) {
				super.instanciated(itemCollection, item);
				item.setName(item.getIdentifiable().getMetricValue().getMetric().getName());
				item.setValue(item.getIdentifiable().getMetricValue().getValue());
			}	
			@Override
			public void write(MetricValueItem item) {
				super.write(item);
				item.getIdentifiable().getMetricValue().setValue(item.getValue());
			}
		});
		metricValueCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
	}
	
	@Override
	public void transfer(UICommand command, Object object) throws Exception {
		super.transfer(command, object);
		metricValueCollection.write();
	}
	
	@Override
	protected void update() {
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().update(identifiable, metricValueCollection.getIdentifiables());
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputNumber private BigDecimal numberOfTimeAbsent;
		@Input @InputText private String appreciation;
		@Input @InputBooleanButton private Boolean conferenceRequested;
		
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
				identifiable.getResults().getLectureAttendance().setMissedDuration(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
						.convertAttendanceTimeToMillisecond(identifiable.getClassroomSessionDivision().getClassroomSession(),numberOfTimeAbsent));
				identifiable.getResults().getLectureAttendance().setAttendedDuration(identifiable.getClassroomSessionDivision().getDuration()-
						identifiable.getResults().getLectureAttendance().getMissedDuration());
			}
			
		}
	}
	
	@Getter @Setter
	public static class MetricValueItem extends AbstractItemCollectionItem<StudentResultsMetricValue> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String name;
		private BigDecimal value;
		@Override
		public String toString() {
			return name+" "+value;
		}
	}

}
