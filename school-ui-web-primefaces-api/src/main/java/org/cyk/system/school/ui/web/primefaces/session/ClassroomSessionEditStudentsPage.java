package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditStudentsPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<StudentClassroomSessionItem, StudentClassroomSession, ClassroomSession> studentClassroomSessionCollection;
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		studentClassroomSessionCollection = createItemCollection(StudentClassroomSessionItem.class, StudentClassroomSession.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<StudentClassroomSessionItem,StudentClassroomSession,ClassroomSession>(identifiable,crud,form,StudentClassroomSession.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_STUDENT_SELECTED;
			}
			
		});
	}
	
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Student oneStudentSelected;
		
		/**/
		
		public static final String FIELD_ONE_STUDENT_SELECTED = "oneStudentSelected";
		
	}
	
	@Getter @Setter
	public static class StudentClassroomSessionItem extends AbstractItemCollectionItem<StudentClassroomSession> {
		private static final long serialVersionUID = 1L;
		
	}

}
