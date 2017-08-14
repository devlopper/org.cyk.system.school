package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionEditStudentsPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<StudentClassroomSessionDivisionItem, StudentClassroomSessionDivision, ClassroomSessionDivision> studentClassroomSessionDivisionCollection;
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		studentClassroomSessionDivisionCollection = createItemCollection(StudentClassroomSessionDivisionItem.class, StudentClassroomSessionDivision.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<StudentClassroomSessionDivisionItem,StudentClassroomSessionDivision,ClassroomSessionDivision>(identifiable,crud,form,StudentClassroomSessionDivision.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public StudentClassroomSessionDivision instanciate(
					AbstractItemCollection<StudentClassroomSessionDivisionItem, StudentClassroomSessionDivision, ClassroomSessionDivision, SelectItem> itemCollection) {
				StudentClassroomSessionDivision studentClassroomSessionDivision = super.instanciate(itemCollection);
				studentClassroomSessionDivision.setCascadeOperationToMaster(Boolean.FALSE);
				studentClassroomSessionDivision.setCascadeOperationToChildren(Boolean.FALSE);
				return studentClassroomSessionDivision;
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_STUDENT_SELECTED;
			}
			
		});
		studentClassroomSessionDivisionCollection.setShowItemLabel(Boolean.TRUE);
	}
	
	@Override
	public Class<?> getFormModelClass() {
		return Form.class;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Student oneStudentSelected;
		
		/**/
		
		public static final String FIELD_ONE_STUDENT_SELECTED = "oneStudentSelected";
		
	}
	
	@Getter @Setter
	public static class StudentClassroomSessionDivisionItem extends AbstractItemCollectionItem<StudentClassroomSessionDivision> {
		private static final long serialVersionUID = 1L;
		
	}

}
