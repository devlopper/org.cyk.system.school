package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class SubjectClassroomSessionAssignToStudentClassroomSessionPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private List<ClassroomSessionSubject> classroomSessionSubjects;
	private ItemCollection<StudentClassroomSessionItem,StudentClassroomSession,ClassroomSession> studentSubjectCollection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		studentSubjectCollection = createItemCollection(StudentClassroomSessionItem.class, StudentClassroomSession.class,null 
				,new ItemCollectionWebAdapter<StudentClassroomSessionItem,StudentClassroomSession,ClassroomSession>(null,crud){
			private static final long serialVersionUID = -3872058204105902514L;
			
			@Override
			public Collection<StudentClassroomSession> create() {
				return webManager.getIdentifiablesFromRequestParameter(StudentClassroomSession.class);
			}
			
			@Override
			public void instanciated(AbstractItemCollection<StudentClassroomSessionItem, StudentClassroomSession,ClassroomSession,SelectItem> itemCollection,StudentClassroomSessionItem item) {
				super.instanciated(itemCollection, item);
				item.setStudent(formatUsingBusiness(item.getIdentifiable().getStudent()));
				item.setClassroomSession(formatUsingBusiness(item.getIdentifiable().getClassroomSession()));
				item.setClassroomSessionSubjectChoices(classroomSessionSubjects = new ArrayList<>(inject(ClassroomSessionSubjectBusiness.class).findByClassroomSession(
						item.getIdentifiable().getClassroomSession())));
				item.setSelectedSubjectClassroomSessions(new ArrayList<>(inject(ClassroomSessionSubjectBusiness.class).findByClassroomSessionByStudent(
						item.getIdentifiable().getClassroomSession(),item.getIdentifiable().getStudent())));
			}
			
			@Override     
			public void write(StudentClassroomSessionItem item) {
				super.write(item);
				Student student = item.getIdentifiable().getStudent();
				item.getIdentifiable().getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
				item.getIdentifiable().getDetailCollection().getCollection().clear();
				for(StudentClassroomSessionDivision studentClassroomSessionDivision : inject(StudentClassroomSessionDivisionBusiness.class)
						.findByStudentByClassroomSession(student, item.getIdentifiable().getClassroomSession())){
					item.getIdentifiable().getDetailCollection().getCollection().add(studentClassroomSessionDivision);
					studentClassroomSessionDivision.getDetailCollection().setSynchonizationEnabled(Boolean.TRUE);
					studentClassroomSessionDivision.getDetailCollection().getCollection().clear();
					for(ClassroomSessionSubject classroomSessionSubject : item.getSelectedSubjectClassroomSessions()){
						StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = inject(StudentClassroomSessionDivisionSubjectBusiness.class)
								.findByStudentByClassroomSessionDivisionBySubject(student, studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject());
						if(studentClassroomSessionDivisionSubject==null)
							studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student
									, inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionBySubject(studentClassroomSessionDivision.getClassroomSessionDivision(),classroomSessionSubject.getSubject()));
						studentClassroomSessionDivision.getDetailCollection().getCollection().add(studentClassroomSessionDivisionSubject);
					}
				}
			}
			
			@Override
			public Crud getCrud() {
				return crud;
			}
			
		});
	}
	
	@Override
	protected <T extends AbstractIdentifiable> T identifiableFromRequestParameter(Class<T> aClass) {
		return null;
	}
	
	/*@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		SchoolWebManager.getInstance().initialiseSelectClassroomSession(this, Form.CLASSROOM_SESSION, Form.CLASSROOM_SESSION_DIVISION
				, Form.CLASSROOM_SESSION_DIVISION_SUBJECT,null);
	}*/
	
	/*@Override
	public void transfer(UICommand command, Object object) throws Exception {
		super.transfer(command, object);
		if(studentSubjectCollection.getAddCommandable().getCommand() == command ){
			form.getSelectedFormData().applyValuesToFields();
		}else{
			studentSubjectCollection.write();
		}
	}*/
	
	@Override
	protected void create() {
		inject(StudentClassroomSessionBusiness.class).update(studentSubjectCollection.getIdentifiables());
		//inject(StudentClassroomSessionDivisionSubjectBusiness.class).create(studentSubjectCollection.getIdentifiables());
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(ClassroomSession.class);
	}
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.CREATE;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private StudentClassroomSession studentClassroomSession;
		
		public static final String CLASSROOM_SESSION = "classroomSession";
		public static final String STUDENT_CLASSROOM_SESSION = "studentClassroomSession";
		
	}
	
	@Getter @Setter
	public static class StudentClassroomSessionItem extends AbstractItemCollectionItem<StudentClassroomSession> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		
		private String student;
		private String classroomSession;
		private List<ClassroomSessionSubject> classroomSessionSubjectChoices;
		private List<ClassroomSessionSubject> selectedSubjectClassroomSessions;
	}
	
}
