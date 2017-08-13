package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
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

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionEditSubjectsPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<ClassroomSessionDivisionSubjectItem, ClassroomSessionDivisionSubject, ClassroomSessionDivision> classroomSessionDivisionSubjectCollection;
	private List<SelectItem> teachers = WebManager.getInstance().getSelectItems(Teacher.class);
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		classroomSessionDivisionSubjectCollection = createItemCollection(ClassroomSessionDivisionSubjectItem.class, ClassroomSessionDivisionSubject.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<ClassroomSessionDivisionSubjectItem,ClassroomSessionDivisionSubject,ClassroomSessionDivision>(identifiable,crud,form,ClassroomSessionDivisionSubject.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_SUBJECT_SELECTED;
			}
			
			@Override
					public IdentifiableRuntimeCollection<ClassroomSessionDivisionSubject> getRuntimeCollection() {
						// TODO Auto-generated method stub
						return identifiable.getClassroomSessionDivisionSubjects();
					}
			
		});
		classroomSessionDivisionSubjectCollection.setShowItemLabel(Boolean.TRUE);
	}
		
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Subject oneSubjectSelected;
		
		/**/
		
		public static final String FIELD_ONE_SUBJECT_SELECTED = "oneSubjectSelected";
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionSubjectItem extends AbstractItemCollectionItem<ClassroomSessionDivisionSubject> {
		private static final long serialVersionUID = 1L;
		
		private Teacher teacher;
		
		@Override
		public void setIdentifiable(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super.setIdentifiable(classroomSessionDivisionSubject);
			teacher = classroomSessionDivisionSubject.getTeacher();
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.setTeacher(teacher);
		}
		
	}

}
