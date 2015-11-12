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
		
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().prepareUpdateOfMetricValues(identifiable);
		
		metricValueCollection = createItemCollection(form, "qwerty", MetricValueItem.class, StudentResultsMetricValue.class, identifiable.getResults().getStudentResultsMetricValues(),new ItemCollectionAdapter<MetricValueItem,StudentResultsMetricValue>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<MetricValueItem, StudentResultsMetricValue> itemCollection,MetricValueItem item) {
				super.instanciated(itemCollection, item);
				item.setName(item.getIdentifiable().getMetricValue().getMetric().getName());
				item.setValue(item.getIdentifiable().getMetricValue().getValue());
			}	
		});
	}
	
	@Override
	public void transfer(UICommand command, Object object) throws Exception {
		super.transfer(command, object);
		for(MetricValueItem item : metricValueCollection.getItems())
			item.getIdentifiable().getMetricValue().setValue(item.getValue());
	}
	
	@Override
	protected void update() {
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().update(identifiable, identifiable.getResults().getStudentResultsMetricValues());
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputNumber private BigDecimal numberOfTimeAbsent;
		@Input @InputText private String appreciation;
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