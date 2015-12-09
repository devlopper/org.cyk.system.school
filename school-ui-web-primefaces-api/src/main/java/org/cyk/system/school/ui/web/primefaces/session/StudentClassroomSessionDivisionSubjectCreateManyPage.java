package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.api.command.UICommand;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.api.model.ItemCollectionListener.ItemCollectionAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionSubjectCreateManyPage extends AbstractCrudOnePage<StudentSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<StudentSubjectItem,StudentSubject> studentSubjectCollection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		//contentTitle = languageBusiness.findClassLabelText(AcademicSession.class)+" : "+identifiable.getAcademicSession().getUiString()
		//		+" - "+SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable);
		studentSubjectCollection = createItemCollection(StudentSubjectItem.class, StudentSubject.class, 
				new ArrayList<StudentSubject>(),new ItemCollectionAdapter<StudentSubjectItem,StudentSubject>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<StudentSubjectItem, StudentSubject> itemCollection,StudentSubjectItem item) {
				super.instanciated(itemCollection, item);
				item.getIdentifiable().setStudent(((Form)form.getData()).getStudent());
				item.getIdentifiable().setClassroomSessionDivisionSubject(((Form)form.getData()).getClassroomSessionDivisionSubject());
				item.setRegistrationCode(item.getIdentifiable().getStudent().getRegistration().getCode());
				item.setNames(item.getIdentifiable().getStudent().getPerson().getNames());
				item.setClassroomSessionDivisionSubject(RootBusinessLayer.getInstance().getFormatterBusiness().format(item.getIdentifiable().getClassroomSessionDivisionSubject()));
			}	
		});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.CLASSROOM_SESSION, SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findByAcademicSession(
				SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null)));

	}
	
	@Override
	public void transfer(UICommand command, Object object) throws Exception {
		super.transfer(command, object);
		if(studentSubjectCollection.getAddCommandable().getCommand() == command ){
			form.getSelectedFormData().applyValuesToFields();
		}else{
			studentSubjectCollection.write();
		}
	}
	
	@Override
	protected void create() {
		SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().create(studentSubjectCollection.getIdentifiables());
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(StudentSubject.class);
	}
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.CREATE;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<StudentSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSessionDivision classroomSessionDivision;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		@Input @InputChoice @InputOneChoice @InputOneCombo private Student student;
		
		public static final String CLASSROOM_SESSION = "classroomSession";
		public static final String CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
		public static final String CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
		public static final String STUDENT = "student";
	}
	
	@Getter @Setter
	public static class StudentSubjectItem extends AbstractItemCollectionItem<StudentSubject> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String registrationCode,names,classroomSession,classroomSessionDivision,classroomSessionDivisionSubject;
	}
	
}
