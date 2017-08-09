package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject, ClassroomSession> classroomSessionSubjectCollection;
	private List<SelectItem> teachers = WebManager.getInstance().getSelectItems(Teacher.class);
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		classroomSessionSubjectCollection = createItemCollection(ClassroomSessionSubjectItem.class, ClassroomSessionSubject.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<ClassroomSessionSubjectItem,ClassroomSessionSubject,ClassroomSession>(identifiable,crud,form){
			private static final long serialVersionUID = 1L;
			
			@Override
			public IdentifiableRuntimeCollection<ClassroomSessionSubject> getRuntimeCollection() {
				return getCollection().getSubjects().setSynchonizationEnabled(Boolean.TRUE);
			}
			
			@Override
			public Collection<ClassroomSessionSubject> findByCollection(ClassroomSession classroomSession) {
				return inject(ClassroomSessionSubjectBusiness.class).findByClassroomSession(classroomSession);
			}
			
			@Override
			public ClassroomSessionSubject instanciate(AbstractItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject, ClassroomSession, SelectItem> itemCollection) {
				return inject(ClassroomSessionSubjectBusiness.class).instanciateOne( (Subject)getInputChoice().getValue(),getCollection() );
			}
			
			@Override
			public void instanciated(AbstractItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject,ClassroomSession,SelectItem> itemCollection,ClassroomSessionSubjectItem item) {
				super.instanciated(itemCollection, item);
				//item.setName(item.getIdentifiable().getSubject().getName());
				//item.setTeacher(item.getIdentifiable().getTeacher());
			}	
			
			@Override
			public void setLabel(AbstractItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject, ClassroomSession, SelectItem> itemCollection,ClassroomSessionSubjectItem item) {
				super.setLabel(itemCollection, item);
				item.setLabel(item.getIdentifiable().getSubject().getName());
			}
								
			@Override
			public AbstractIdentifiable getMasterSelected(AbstractItemCollection<ClassroomSessionSubjectItem, ClassroomSessionSubject,ClassroomSession, SelectItem> itemCollection,
					ClassroomSessionSubject classroomSessionSubject) {
				return classroomSessionSubject.getSubject();
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_SUBJECT_SELECTED;
			}
			
			@Override
			public void write(ClassroomSessionSubjectItem item) {
				super.write(item);
				//System.out.println(item.getTeacher().getCode());
				//item.getIdentifiable().setTeacher(item.getTeacher());
				//System.out.println(item.getIdentifiable().getTeacher());
			}
		});
	}
	
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private AcademicSession academicSession;
		@Input @InputChoice @InputOneChoice @InputOneCombo private LevelTimeDivision levelTimeDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSessionSuffix suffix;
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Teacher coordinator;
		
		@IncludeInputs private CommonNodeInformationsFormModel nodeInformations;
		 
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Subject oneSubjectSelected;
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Student oneStudentSelected;
		
		@Override
		public void read() {
			super.read();
			
		}
		
		@Override
		public void write() {
			super.write();
			
		}
		
		/**/
		
		public static final String FIELD_ACADEMIC_SESSION = "academicSession";
		public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
		public static final String FIELD_SUFFIX = "suffix";
		public static final String FIELD_COORDINATOR = "coordinator";
		public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
		public static final String FIELD_ONE_SUBJECT_SELECTED = "oneSubjectSelected";
		public static final String FIELD_ONE_STUDENT_SELECTED = "oneStudentSelected";
		
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
