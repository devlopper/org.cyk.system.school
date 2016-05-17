package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractEditManyPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditManyEvaluationAveragePage extends AbstractEditManyPage<StudentClassroomSessionDivision,StudentClassroomSessionDivisionEditManyEvaluationAveragePage.Form> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected Class<Form> getItemCollectionItemClass() {
		return Form.class;
	}
	
	@Override
	protected void create() {
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().update(elementCollection.getIdentifiables());
	}
	
	@Override
	public ItemCollectionWebAdapter<Form, StudentClassroomSessionDivision> getItemCollectionAdapter() {
		return new ItemCollectionAdapter<StudentClassroomSessionDivision,Form>(businessEntityInfos){
			private static final long serialVersionUID = -5381415970572336750L;
			
			@Override
			public void instanciated(AbstractItemCollection<Form, StudentClassroomSessionDivision, SelectItem> itemCollection,Form item) {
				super.instanciated(itemCollection, item);
				item.setValue(item.getIdentifiable().getResults().getEvaluationSort().getAverage().getValue());
			}
			
			@Override
			public void write(Form item) {
				super.write(item);
				item.getIdentifiable().getResults().getEvaluationSort().getAverage().setValue(item.getValue());
			}
		};
	}
	
	/**/
	
	@Getter @Setter
	public static class Form extends AbstractItemCollectionItem<StudentClassroomSessionDivision> implements Serializable{
		
		private static final long serialVersionUID = -829786138986362643L;

		@Input @InputNumber private BigDecimal value;
		
	}
}
