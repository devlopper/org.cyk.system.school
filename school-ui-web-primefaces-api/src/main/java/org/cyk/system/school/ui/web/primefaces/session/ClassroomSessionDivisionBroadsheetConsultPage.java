package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.api.model.table.Cell;
import org.cyk.ui.api.model.table.CellAdapter;
import org.cyk.ui.api.model.table.Column;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.api.model.table.RowAdapter;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionBroadsheetConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<String> table;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		table = new Table<>();
		tables.add(table);
		table.setLazyLoad(Boolean.FALSE);
		
		table.setShowHeader(Boolean.FALSE);
		table.setShowFooter(Boolean.FALSE);
		
		final List<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivision(identifiable));
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			Column column = new Column();
			column.setTitle(classroomSessionDivisionSubject.getSubject().getName());
			table.addColumn(column);
		}
		
		final List<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>(SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivision(identifiable));
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
			table.getInitialData().add(formatUsingBusiness(studentClassroomSessionDivision.getStudent()));
		}
		
		table.getRowListeners().add(new RowAdapter<String>(){
			@Override
			public void added(Row<String> row) {
				super.added(row);
				
			}
		});
		
		table.getCellListeners().add(new CellAdapter<String>(){
			@Override
			public void added(Row<String> row, Column column, Cell cell) {
				super.added(row, column, cell);
				cell.setValue(numberBusiness.format(SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findByStudentByClassroomSessionDivisionSubject(
						studentClassroomSessionDivisions.get(row.getIndex().intValue()).getStudent(),
						classroomSessionDivisionSubjects.get(column.getIndex().intValue())).getResults().getEvaluationSort().getAverage().getValue()));
			}
		});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
				
		//table.setShowHeader(Boolean.FALSE);
		//table.setShowFooter(Boolean.FALSE);
		//table.setShowToolBar(Boolean.FALSE);
		
		table.build();
		table.postConstruct();
		onDocumentLoadJavaScript += tableFormatJavaScript(table, Boolean.TRUE);
		
		System.out.println(onDocumentLoadJavaScript);
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(ClassroomSessionDivision.class);
	}
	
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.READ;
	}

}
