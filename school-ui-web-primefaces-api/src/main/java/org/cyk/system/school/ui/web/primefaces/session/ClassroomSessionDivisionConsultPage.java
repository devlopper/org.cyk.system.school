package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.api.model.table.Cell;
import org.cyk.ui.api.model.table.CellAdapter;
import org.cyk.ui.api.model.table.Column;
import org.cyk.ui.api.model.table.ColumnAdapter;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<ClassroomSessionDivisionDetails> details;
	private Table<ClassroomSessionDivisionSubjectDetails> subjectTable;
	private Table<StudentClassroomSessionDivisionDetails> studentTable;
	private Table<StudentClassroomSessionDivisionDetails> broadsheetTable;
	
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
		studentTable.getColumnListeners().add(new ColumnAdapter(){
			@Override
			public Boolean isColumn(Field field) {
				return StudentClassroomSessionDivisionDetails.FIELDS_SIMPLE.contains(field.getName());
			}
		});
		
		final List<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivision(identifiable));
		broadsheetTable = (Table<StudentClassroomSessionDivisionDetails>) createDetailsTable(StudentClassroomSessionDivisionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails>(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivision> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivision(identifiable);
			}	
			@Override
			public Collection<StudentClassroomSessionDivisionDetails> getDatas() {
				Collection<StudentClassroomSessionDivisionDetails> datas = super.getDatas();
				StudentClassroomSessionDivisionDetails s = new StudentClassroomSessionDivisionDetails(null);
				s.setIdentifier(StudentClassroomSessionDivisionDetails.IDENTIFIER_1);
				s.setNames(text("school.report.broadsheet.average"));
				datas.add(s);
				s = new StudentClassroomSessionDivisionDetails(null);
				s.setIdentifier(StudentClassroomSessionDivisionDetails.IDENTIFIER_2);
				s.setNames(text("school.report.broadsheet.passfraction"));
				datas.add(s);
				s = new StudentClassroomSessionDivisionDetails(null);
				s.setIdentifier(StudentClassroomSessionDivisionDetails.IDENTIFIER_3);
				s.setNames(text("school.report.broadsheet.passpercentage"));
				datas.add(s);
				return datas;
			}
			@Override
			public String getTitleId() {
				return "school.broadsheet";
			}
			
		});
		
		broadsheetTable.getColumnListeners().add(new ColumnAdapter(){
			@Override
			public Boolean isColumn(Field field) {
				if(StudentClassroomSessionDivisionDetails.FIELDS_BROAD_SHEET.contains(field.getName())){
					if(StudentClassroomSessionDivisionDetails.isSubjectAverageFieldName(field.getName()))
						return StudentClassroomSessionDivisionDetails.getSubjectAverageFieldNameIndex(field.getName()) < classroomSessionDivisionSubjects.size();
					else
						return Boolean.TRUE;
				}else
					return Boolean.FALSE;
			}
			
			@Override
			public void added(Column column) {
				super.added(column);
				if(column.getField().getName().startsWith("subject")){
					column.setTitle(classroomSessionDivisionSubjects.get(
							StudentClassroomSessionDivisionDetails.getSubjectAverageFieldNameIndex(column.getField().getName())).getSubject().getName());
					
				}
			}
		});
		
		final Integer numberOfColumnBeforeSubjects = 2;
		final List<StudentSubject> studentSubjects = new ArrayList<>(SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findByClassroomSessionDivision(identifiable));
		broadsheetTable.getCellListeners().add(new CellAdapter<StudentClassroomSessionDivisionDetails>(){
			@Override
			public void added(Row<StudentClassroomSessionDivisionDetails> row, Column column, Cell cell) {
				super.added(row, column, cell);
				if(column.getIndex() < classroomSessionDivisionSubjects.size()+numberOfColumnBeforeSubjects){
					if(row.getData().getMaster()==null){
						if(column.getIndex() >= numberOfColumnBeforeSubjects && column.getIndex() < classroomSessionDivisionSubjects.size()+numberOfColumnBeforeSubjects){
							ClassroomSessionDivisionSubject classroomSessionDivisionSubject = classroomSessionDivisionSubjects.get(column.getIndex().intValue()-numberOfColumnBeforeSubjects);
							if(row.getData().getIdentifier().equals(StudentClassroomSessionDivisionDetails.IDENTIFIER_1))
								cell.setValue(numberBusiness.format(classroomSessionDivisionSubject.getResults().getAverage()));
							else if(row.getData().getIdentifier().equals(StudentClassroomSessionDivisionDetails.IDENTIFIER_2))
								if(classroomSessionDivisionSubject.getResults().getNumberOfStudentPassingEvaluationAverage()!=null)
									cell.setValue(classroomSessionDivisionSubject.getResults().getNumberOfStudentPassingEvaluationAverage()+Constant.CHARACTER_SLASH.toString()+
										classroomSessionDivisionSubject.getResults().getNumberOfStudent());
							else if(row.getData().getIdentifier().equals(StudentClassroomSessionDivisionDetails.IDENTIFIER_3)){
								NumberBusiness.FormatArguments formatArguments = new FormatArguments();
								formatArguments.setIsPercentage(Boolean.TRUE);
								cell.setValue(numberBusiness.format(classroomSessionDivisionSubject.getResults().getNumberOfStudentPassingEvaluationAverage()/
										classroomSessionDivisionSubject.getResults().getNumberOfStudent(),formatArguments));
							}
						}
					}else{
						StudentSubject studentSubject = null;
						for(StudentSubject ss : studentSubjects)
							if(ss.getStudent().equals(row.getData().getMaster().getStudent()) ){
								if(classroomSessionDivisionSubjects.indexOf(ss.getClassroomSessionDivisionSubject()) + numberOfColumnBeforeSubjects == column.getIndex()){
									studentSubject = ss;
									break;
								}
							}
						if(studentSubject!=null){
							cell.setValue(numberBusiness.format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()));
						}
					}
				}
			}
		});
		broadsheetTable.setUpdateStyleClass("broadsheetTableStyleClass");
	}
	
	@Override
	protected String getContentTitleIdentifiableText() {
		return formatUsingBusiness(new Object[]{identifiable.getClassroomSession(),identifiable});
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
