package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
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
public class StudentClassroomSessionEditSubjectsPage extends AbstractCrudOnePage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<StudentClassroomSessionSubjectItem, StudentClassroomSessionSubject, StudentClassroomSession> studentClassroomSessionSubjectCollection;
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		@SuppressWarnings("unchecked")
		org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem> input = (org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem>)
				form.getInputByFieldName(Form.FIELD_ONE_CLASSROOM_SESSION_SUBJECT_SELECTED);
		
		input.getList().addAll(WebManager.getInstance().getSelectItems(ClassroomSessionSubject.class,inject(ClassroomSessionSubjectBusiness.class).findByClassroomSession(identifiable.getClassroomSession()),Boolean.FALSE));
		
		studentClassroomSessionSubjectCollection = createItemCollection(StudentClassroomSessionSubjectItem.class, StudentClassroomSessionSubject.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<StudentClassroomSessionSubjectItem,StudentClassroomSessionSubject,StudentClassroomSession>(identifiable,crud,form,StudentClassroomSessionSubject.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public void instanciated(AbstractItemCollection<StudentClassroomSessionSubjectItem, StudentClassroomSessionSubject, StudentClassroomSession, SelectItem> itemCollection,StudentClassroomSessionSubjectItem item) {
				super.instanciated(itemCollection, item);
				item.getIdentifiable().setCascadeOperationToChildren(Boolean.TRUE);
				item.getIdentifiable().setCascadeOperationToMaster(Boolean.FALSE);
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_CLASSROOM_SESSION_SUBJECT_SELECTED;
			}
			
		});
		studentClassroomSessionSubjectCollection.setShowItemLabel(Boolean.TRUE);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
		
	public static class Form extends AbstractFormModel<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(load=false,nullable=false) @InputOneChoice @InputOneCombo protected ClassroomSessionSubject oneClassroomSessionSubjectSelected;
		
		/**/
		
		public static final String FIELD_ONE_CLASSROOM_SESSION_SUBJECT_SELECTED = "oneClassroomSessionSubjectSelected";
	}
	
	@Getter @Setter
	public static class StudentClassroomSessionSubjectItem extends AbstractItemCollectionItem<StudentClassroomSessionSubject> {
		private static final long serialVersionUID = 1L;
		
	}
	
}
