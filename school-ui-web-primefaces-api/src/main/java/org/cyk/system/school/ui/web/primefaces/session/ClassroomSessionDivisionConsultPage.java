package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<ClassroomSessionDivisionDetails> details;
	private Table<ClassroomSessionDivisionSubjectDetails> subjectTable;
	private Table<StudentClassroomSessionDivisionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		details = createDetailsForm(ClassroomSessionDivisionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails>(ClassroomSessionDivision.class, ClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		subjectTable = (Table<ClassroomSessionDivisionSubjectDetails>) createDetailsTable(ClassroomSessionDivisionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivisionSubject,ClassroomSessionDivisionSubjectDetails>(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivisionSubject> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivision(identifiable);
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		studentTable = (Table<StudentClassroomSessionDivisionDetails>) createDetailsTable(StudentClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails>(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionDetails.class){
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
	protected void processIdentifiableContextualCommandable(UICommandable commandable) {
		super.processIdentifiableContextualCommandable(commandable);
		commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable, "command.selectclassroomsessiondivision.auscsdr", null,
				SchoolWebManager.getInstance().getOutcomeUpdateStudentClassroomSessionDivisionResults()));
		
		commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable, "school.markscard.generate", null,
				SchoolWebManager.getInstance().getOutcomeGenerateStudentClassroomSessionDivisionReport()));
	}
	
}
