package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.Subject;
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
public class ClassroomSessionEditSubjectsPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject, ClassroomSession> classroomSessionSubjectCollection;
	private List<SelectItem> teachers = WebManager.getInstance().getSelectItems(Teacher.class);
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		classroomSessionSubjectCollection = createItemCollection(ClassroomSessionSubjectItem.class, ClassroomSessionSubject.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<ClassroomSessionSubjectItem,ClassroomSessionSubject,ClassroomSession>(identifiable,crud,form,ClassroomSessionSubject.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public void instanciated(AbstractItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject, ClassroomSession, SelectItem> itemCollection,ClassroomSessionSubjectItem item) {
				super.instanciated(itemCollection, item);
				item.getIdentifiable().setCascadeOperationToChildren(Boolean.TRUE);
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_SUBJECT_SELECTED;
			}
			
		});
		classroomSessionSubjectCollection.setShowItemLabel(Boolean.TRUE);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
		
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Subject oneSubjectSelected;
		
		/**/
		
		public static final String FIELD_ONE_SUBJECT_SELECTED = "oneSubjectSelected";
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionSubjectItem extends AbstractItemCollectionItem<ClassroomSessionSubject> {
		private static final long serialVersionUID = 1L;
		
		private Teacher teacher;
		
		@Override
		public void setIdentifiable(ClassroomSessionSubject classroomSessionSubject) {
			super.setIdentifiable(classroomSessionSubject);
			teacher = classroomSessionSubject.getTeacher();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.setTeacher(teacher);
		}
		
	}

}
