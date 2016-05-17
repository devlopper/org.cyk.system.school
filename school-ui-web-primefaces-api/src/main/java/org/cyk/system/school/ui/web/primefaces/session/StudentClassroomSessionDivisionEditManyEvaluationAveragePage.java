package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractEditManyPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditManyEvaluationAveragePage extends AbstractEditManyPage<StudentClassroomSessionDivision,StudentClassroomSessionDivisionEditManyEvaluationAveragePage.FormModel> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		elementCollection.getItemCollectionListeners().add(new ItemCollectionWebAdapter<FormModel,StudentClassroomSessionDivision>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<FormModel, StudentClassroomSessionDivision,SelectItem> itemCollection,FormModel item) {
				super.instanciated(itemCollection, item);
				item.setValue(item.getIdentifiable().getResults().getEvaluationSort().getAverage().getValue());
				debug(item.getIdentifiable().getResults().getEvaluationSort());
			}	
		});
	}
	
	@Override
	protected Class<FormModel> getItemCollectionItemClass() {
		return FormModel.class;
	}
	
	@Override
	public ItemCollectionWebAdapter<FormModel, StudentClassroomSessionDivision> getItemCollectionAdapter() {
		return new CustomItemCollectionAdapter();
	}
	
	/**/
	
	public static class CustomItemCollectionAdapter extends ItemCollectionAdapter<StudentClassroomSessionDivision,FormModel>  implements Serializable {
		private static final long serialVersionUID = 7806030819027062650L;
		@Override
		public void instanciated(AbstractItemCollection<FormModel, StudentClassroomSessionDivision, SelectItem> itemCollection,FormModel item) {
			super.instanciated(itemCollection, item);
			item.setValue(item.getIdentifiable().getResults().getEvaluationSort().getAverage().getValue());
			
			System.out.println(
					"StudentClassroomSessionDivisionEditManyEvaluationAveragePage.CustomItemCollectionAdapter.instanciated()");
			debug(item.getIdentifiable().getResults().getEvaluationSort());
		}
		
		@Override
		public void write(FormModel item) {
			super.write(item);
			item.getIdentifiable().getResults().getEvaluationSort().getAverage().setValue(item.getValue());
			System.out.println(
					"StudentClassroomSessionDivisionEditManyEvaluationAveragePage.CustomItemCollectionAdapter.write()");
			debug(item);
		}
	}
	
	@Getter @Setter
	public static class FormModel extends AbstractItemCollectionItem<StudentClassroomSessionDivision> implements Serializable{
		
		private static final long serialVersionUID = -829786138986362643L;

		@Input @InputNumber private BigDecimal value;
		
	}
}
