package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.ui.api.model.table.AbstractTable;
import org.cyk.ui.web.primefaces.HierarchyNode;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.Table.Listener;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudManyPage;
import org.primefaces.model.TreeNode;

@Named @ViewScoped @Getter @Setter
public class EvaluationListPage extends AbstractCrudManyPage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	@Override
	protected Listener<Object> getTableListener() {
		return new Table.Listener.Adapter<Object>(){
			private static final long serialVersionUID = 1L;
			
			@Override
			public CreateCommandableArguments getCreateCommandableArguments(Commandable commandable) {
				switch(commandable){
				case ADD: 
					return new CreateCommandableArguments().select(ClassroomSessionDivisionSubjectEvaluationType.class
							, SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation(),Boolean.TRUE);
				}
				return null;
			}
			
			@Override
			public AbstractTable<Object, TreeNode, HierarchyNode> getTable() {
				return table;
			}
		};
	}
}
