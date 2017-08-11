package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditDivisionsPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<ClassroomSessionDivisionItem, ClassroomSessionDivision, ClassroomSession> classroomSessionDivisionCollection;
	private List<SelectItem> timeDivisionTypes = WebManager.getInstance().getSelectItems(TimeDivisionType.class);
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		@SuppressWarnings("unchecked")
		org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem> input = (org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem>)
				form.getInputByFieldName(Form.FIELD_ONE_ORDER_NUMBER_SELECTED);
		input.getList().add(new SelectItem(new Long(1), "1"));
		input.getList().add(new SelectItem(new Long(2), "2"));
		input.getList().add(new SelectItem(new Long(3), "3"));
		
		classroomSessionDivisionCollection = createItemCollection(ClassroomSessionDivisionItem.class, ClassroomSessionDivision.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<ClassroomSessionDivisionItem,ClassroomSessionDivision,ClassroomSession>(identifiable,crud,form,ClassroomSessionDivision.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public IdentifiableRuntimeCollection<ClassroomSessionDivision> getRuntimeCollection() {
				return identifiable.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
			}
			
			@Override
			public Object getMasterSelected(AbstractItemCollection<ClassroomSessionDivisionItem, ClassroomSessionDivision, ClassroomSession, SelectItem> itemCollection,
					ClassroomSessionDivision classroomSessionDivision) {
				return classroomSessionDivision.getOrderNumber();
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_ORDER_NUMBER_SELECTED;
			}
			
		});
	}

	@Override
	protected void update() {
		identifiable.getDivisions().setCollection(classroomSessionDivisionCollection.getIdentifiables());
		super.update();
	}
	
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Long oneOrderNumberSelected;
		
		/**/
		
		public static final String FIELD_ONE_ORDER_NUMBER_SELECTED = "oneOrderNumberSelected";
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionItem extends AbstractItemCollectionItem<ClassroomSessionDivision> {
		private static final long serialVersionUID = 1L;
		
		private TimeDivisionType timeDivisionType;
		
		@Override
		public void setIdentifiable(ClassroomSessionDivision classroomSessionDivision) {
			super.setIdentifiable(classroomSessionDivision);
			timeDivisionType = classroomSessionDivision.getTimeDivisionType();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.setTimeDivisionType(timeDivisionType);
		}
		
	}

}
