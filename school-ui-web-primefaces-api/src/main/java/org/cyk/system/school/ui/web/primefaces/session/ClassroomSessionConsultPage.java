package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.business.impl.subject.SubjectClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<ClassroomSessionDetails> details;
	private Table<ClassroomSessionDivisionDetails> divisionTable;
	private Table<SubjectClassroomSessionDetails> subjectTable;
	private Table<StudentClassroomSessionDetails> studentTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		details = createDetailsForm(ClassroomSessionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<ClassroomSession,ClassroomSessionDetails>(ClassroomSession.class, ClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		divisionTable = (Table<ClassroomSessionDivisionDetails>) createDetailsTable(ClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivision,ClassroomSessionDivisionDetails>(ClassroomSessionDivision.class, ClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivision> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(identifiable);
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		subjectTable = (Table<SubjectClassroomSessionDetails>) createDetailsTable(SubjectClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<SubjectClassroomSession,SubjectClassroomSessionDetails>(SubjectClassroomSession.class, SubjectClassroomSessionDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public Collection<SubjectClassroomSession> getIdentifiables() {
					return SchoolBusinessLayer.getInstance().getSubjectClassroomSessionBusiness().findByClassroomSession(identifiable);
				}
				@Override
				public Crud[] getCruds() {
					return new Crud[]{Crud.READ,Crud.UPDATE};
				}
			});
		
		studentTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>(StudentClassroomSession.class, StudentClassroomSessionDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public Collection<StudentClassroomSession> getIdentifiables() {
					return SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().findByClassroomSession(identifiable);
				}
				@Override
				public Crud[] getCruds() {
					return new Crud[]{Crud.CREATE,Crud.READ,Crud.UPDATE,Crud.DELETE};
				}
			});
					
	}

}
