package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.business.api.session.SubjectClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
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

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<SubjectClassroomSessionItem, SubjectClassroomSession, ClassroomSession> subjectClassroomSessionCollection;
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		subjectClassroomSessionCollection = createItemCollection(SubjectClassroomSessionItem.class, SubjectClassroomSession.class,identifiable 
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<SubjectClassroomSessionItem,SubjectClassroomSession,ClassroomSession>(identifiable,crud,form){
			private static final long serialVersionUID = 1L;
			
			@Override
			public IdentifiableRuntimeCollection<SubjectClassroomSession> getRuntimeCollection() {
				return getCollection().getSubjects().setSynchonizationEnabled(Boolean.TRUE);
			}
			
			@Override
			public SubjectClassroomSession instanciate(AbstractItemCollection<SubjectClassroomSessionItem, SubjectClassroomSession, ClassroomSession, SelectItem> itemCollection) {
				return inject(SubjectClassroomSessionBusiness.class).instanciateOne( (Subject)getInputChoice().getValue(),getCollection() );
			}
			
			@Override
			public void instanciated(AbstractItemCollection<SubjectClassroomSessionItem, SubjectClassroomSession,ClassroomSession,SelectItem> itemCollection,SubjectClassroomSessionItem item) {
				super.instanciated(itemCollection, item);
				//item.setName(item.getIdentifiable().getSubject().getName());
			}	
			
			@Override
			public void setLabel(AbstractItemCollection<SubjectClassroomSessionItem, SubjectClassroomSession, ClassroomSession, SelectItem> itemCollection,SubjectClassroomSessionItem item) {
				super.setLabel(itemCollection, item);
				item.setLabel(item.getIdentifiable().getSubject().getName());
			}
								
			@Override
			public AbstractIdentifiable getMasterSelected(AbstractItemCollection<SubjectClassroomSessionItem, SubjectClassroomSession,ClassroomSession, SelectItem> itemCollection,
					SubjectClassroomSession subjectClassroomSession) {
				return subjectClassroomSession.getSubject();
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_SUBJECT_SELECTED;
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
		
	}
	
	public static class SubjectClassroomSessionItem extends AbstractItemCollectionItem<SubjectClassroomSession> {
		private static final long serialVersionUID = 1L;
		
	}

}
