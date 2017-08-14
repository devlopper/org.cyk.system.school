package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionEditClassroomSessionsPage extends AbstractCrudOnePage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<ClassroomSessionItem, ClassroomSession, AcademicSession> classroomSessionCollection;
	private List<SelectItem> coordinators = WebManager.getInstance().getSelectItems(Teacher.class);
	private List<SelectItem> suffixes = WebManager.getInstance().getSelectItems(ClassroomSessionSuffix.class);
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		classroomSessionCollection = createItemCollection(ClassroomSessionItem.class, ClassroomSession.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<ClassroomSessionItem,ClassroomSession,AcademicSession>(identifiable,crud,form,ClassroomSession.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_LEVEL_TIME_DIVISION_SELECTED;
			}
			
			@Override
			public Boolean getIsInputChoiceUnique() {
				return Boolean.FALSE;
			}
			
		});
		classroomSessionCollection.setShowItemLabel(Boolean.TRUE);

	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
		
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<AcademicSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected LevelTimeDivision oneLevelTimeDivisionSelected;
		
		/**/
		
		public static final String FIELD_ONE_LEVEL_TIME_DIVISION_SELECTED = "oneLevelTimeDivisionSelected";
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionItem extends AbstractItemCollectionItem<ClassroomSession> {
		private static final long serialVersionUID = 1L;
		
		private ClassroomSessionSuffix suffix;
		private Teacher coordinator;
		
		@Override
		public void setIdentifiable(ClassroomSession classroomSession) {
			super.setIdentifiable(classroomSession);
			suffix = classroomSession.getSuffix();
			coordinator = classroomSession.getCoordinator();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.setSuffix(suffix);
			identifiable.setCoordinator(coordinator);
		}
		
	}
}
