package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<Details> details;
	private Table<SubjectDetails> subjectTable;
	private Table<StudentDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(identifiable);
		
		details = createDetailsForm(Details.class, identifiable, new DetailsFormOneDataConfigurationAdapter<ClassroomSessionDivision,Details>(ClassroomSessionDivision.class, Details.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		subjectTable = (Table<SubjectDetails>) createDetailsTable(SubjectDetails.class, new DetailsTableConfigurationAdapter<ClassroomSessionDivisionSubject,SubjectDetails>(ClassroomSessionDivisionSubject.class, SubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivisionSubject> getIdentifiables() {
				return identifiable.getSubjects();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		studentTable = (Table<StudentDetails>) createDetailsTable(StudentDetails.class, new DetailsTableConfigurationAdapter<StudentClassroomSessionDivision,StudentDetails>(StudentClassroomSessionDivision.class, StudentDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivision> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivision(identifiable);
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSession(), "button", null);
		commandable.setLabel(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable.getClassroomSession()));
		contextualMenu.getChildren().add(commandable);
		
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String name,duration;
		public Details(ClassroomSessionDivision classroomSessionDivision) {
			super(classroomSessionDivision);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(classroomSessionDivision);
			duration = timeBusiness.formatDuration(classroomSessionDivision.getDuration());
		}
	}
	
	public static class SubjectDetails extends AbstractOutputDetails<ClassroomSessionDivisionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String name,coefficient,teacher;
		public SubjectDetails(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super(classroomSessionDivisionSubject);
			name = classroomSessionDivisionSubject.getSubject().getName();
			coefficient = numberBusiness.format(classroomSessionDivisionSubject.getCoefficient());
			teacher = classroomSessionDivisionSubject.getTeacher().getPerson().getNames();
		}
	}
	
	public static class StudentDetails extends AbstractOutputDetails<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String names;
		public StudentDetails(StudentClassroomSessionDivision studentClassroomSessionDivision) {
			super(studentClassroomSessionDivision);
			names = studentClassroomSessionDivision.getStudent().getPerson().getNames();
		}
	}

}
