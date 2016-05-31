package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractClassLevelConsultPage<ClassroomSessionDivision,ClassroomSessionDivisionDetails,ClassroomSessionDivisionSubject,ClassroomSessionDivisionSubjectDetails,StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails,StudentClassroomSessionDivisionSubject,StudentClassroomSessionDivisionSubjectDetails> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	@Override
	protected String getContentTitleIdentifiableText() {
		return formatUsingBusiness(new Object[]{identifiable.getClassroomSession(),identifiable});
	}

	@Override
	protected Class<ClassroomSessionDivision> getLevelClass() {
		return ClassroomSessionDivision.class;
	}

	@Override
	protected Class<ClassroomSessionDivisionDetails> getLevelOutputClass() {
		return ClassroomSessionDivisionDetails.class;
	}

	@Override
	protected Class<ClassroomSessionDivisionSubject> getSubLevelClass() {
		return ClassroomSessionDivisionSubject.class;
	}

	@Override
	protected Class<ClassroomSessionDivisionSubjectDetails> getSubLevelOutputClass() {
		return ClassroomSessionDivisionSubjectDetails.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivisionSubject> getDetailClass() {
		return StudentClassroomSessionDivisionSubject.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivisionSubjectDetails> getDetailOutputClass() {
		return StudentClassroomSessionDivisionSubjectDetails.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivision> getResultClass() {
		return StudentClassroomSessionDivision.class;
	}

	@Override
	protected Class<StudentClassroomSessionDivisionDetails> getResultOutputClass() {
		return StudentClassroomSessionDivisionDetails.class;
	}

	@Override
	protected ClassroomSession getClassroomSession() {
		return identifiable.getClassroomSession();
	}

	@Override
	protected Collection<ClassroomSessionDivisionSubject> getSubLevels() {
		if(Boolean.TRUE.equals(userSession.getIsManager()) || isCoordinator)
			return SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivision(identifiable);
		else
			if(teacher==null)
				return null;
			else
				return SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivisionByTeacher(identifiable,teacher);
	}

	@Override
	protected Collection<StudentClassroomSessionDivision> getResults() {
		return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivision(identifiable);
	}

	@Override
	protected Set<String> getResultTableSimpleFieldNameSet() {
		return StudentClassroomSessionDivisionDetails.FIELDS_SIMPLE;
	}

	@Override
	protected Set<String> getResultTableBroadsheetFieldNameSet() {
		return StudentClassroomSessionDivisionDetails.FIELDS_BROAD_SHEET;
	}
	
	@Override
	protected CellAdapter<ClassroomSessionDivisionSubject, StudentClassroomSessionDivisionSubject, StudentClassroomSessionDivisionDetails> getBroadsheetTableCellAdapter(List<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects) {
		return new CellAdapter<ClassroomSessionDivisionSubject, StudentClassroomSessionDivisionSubject, StudentClassroomSessionDivisionDetails>(2,classroomSessionDivisionSubjects) {
			private static final long serialVersionUID = 7951604788207259255L;
			@Override
			protected ClassroomSessionDivisionSubject getSubLevel(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
				return studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject();
			}
			
			@Override
			protected NodeResults getNodeResults(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
				return classroomSessionDivisionSubject.getResults();
			}
			
			@Override
			protected Collection<StudentClassroomSessionDivisionSubject> getDetailCollection() {
				if(Boolean.TRUE.equals(userSession.getIsManager()) || isCoordinator)
					return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivision(identifiable);
				else
					if(teacher==null)
						return null;
					else
						return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivisionByTeacher(identifiable,teacher);
			}
		};
	}
	
	/*
	@Override
	protected void processIdentifiableContextualCommandable(UICommandable commandable) {
		super.processIdentifiableContextualCommandable(commandable);
		commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable, "command.selectclassroomsessiondivision.auscsdr", null,
				SchoolWebManager.getInstance().getOutcomeUpdateStudentClassroomSessionDivisionResults()));
		
		commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable, "school.markscard.generate", null,
				SchoolWebManager.getInstance().getOutcomeGenerateStudentClassroomSessionDivisionReport()));
	}
	*/

}
