package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.FormatterBusiness;
import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsOutputDetails;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.model.table.Cell;
import org.cyk.ui.api.model.table.Column;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.Constant;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public abstract class AbstractClassLevelConsultPage<LEVEL extends AbstractIdentifiable,LEVEL_OUTPUT,SUB_LEVEL extends AbstractIdentifiable,SUB_LEVEL_OUTPUT,RESULT extends AbstractStudentResult<LEVEL, DETAIL>,RESULT_OUTPUT extends AbstractStudentResultsOutputDetails<LEVEL, RESULT, DETAIL>,DETAIL extends AbstractStudentResult<?, ?>,DETAIL_OUTPUT> extends AbstractConsultPage<LEVEL> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	protected Teacher teacher;
	protected Boolean isCoordinator;
	protected List<SUB_LEVEL> subLevels;
	protected Integer numberOfColumnBeforeLevels = 2;
	
	protected Table<SUB_LEVEL_OUTPUT> subLevelTable;
	protected Table<RESULT_OUTPUT> studentTable;
	protected Table<RESULT_OUTPUT> broadsheetTable;
	
	@Override
	protected void consultInitialisation() {
		super.consultInitialisation();
		teacher = userSession.getUser() instanceof Person ? inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser()) : null;
		isCoordinator = teacher != null && getClassroomSession().getCoordinator()!= null && teacher.equals( getClassroomSession().getCoordinator());
		
		/*details = createDetailsForm(getLevelOutputClass(), identifiable, new DetailsConfigurationListener.Form.Adapter<LEVEL,LEVEL_OUTPUT>(getLevelClass(), getLevelOutputClass()){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});*/
		
		subLevelTable = (Table<SUB_LEVEL_OUTPUT>) createDetailsTable(getSubLevelOutputClass(), getSubLevelTableCreationListener());
		
		studentTable = (Table<RESULT_OUTPUT>) createDetailsTable(getResultOutputClass(), getResultTableCreationListener());
		studentTable.getColumnListeners().add(getResultTableColumnAdapter());
		
		if(SchoolConstant.UI_TAB_BROADSHEET_ID.equals(selectedTabId)){
			subLevels = new ArrayList<>(getSubLevels());
		}
		
		broadsheetTable = (Table<RESULT_OUTPUT>) createDetailsTable(getResultOutputClass(), new DetailsConfigurationListener.Table.Adapter<RESULT,RESULT_OUTPUT>(getResultClass(), getResultOutputClass()){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<RESULT> getIdentifiables() {
				return getResults();
			}	
			@Override
			public Collection<RESULT_OUTPUT> getDatas() {
				Collection<RESULT_OUTPUT> datas = super.getDatas();
				RESULT_OUTPUT s = newInstance(getResultOutputClass());
				s.setIdentifier(AbstractOutputDetails.IDENTIFIER_1);
				s.setStudent(text("school.report.broadsheet.average"));
				datas.add(s);
				s = newInstance(getResultOutputClass());
				s.setIdentifier(AbstractOutputDetails.IDENTIFIER_2);
				s.setStudent(text("school.report.broadsheet.passfraction"));
				datas.add(s);
				s = newInstance(getResultOutputClass());
				s.setIdentifier(AbstractOutputDetails.IDENTIFIER_3);
				s.setStudent(text("school.report.broadsheet.passpercentage")+Constant.CHARACTER_SPACE+Constant.CHARACTER_LEFT_PARENTHESIS+Constant.CHARACTER_PERCENT
						+Constant.CHARACTER_RIGHT_PARENTHESIS);
				datas.add(s);
				return datas;
			}
			@Override
			public String getTitleId() {
				return SchoolConstant.UI_TAB_BROADSHEET_ID;
			}
			
		});
		
		broadsheetTable.getColumnListeners().add(new ColumnAdapter<>(userSession, teacher, isCoordinator, getSubLevelClass(), subLevels, 2, getResultTableBroadsheetFieldNameSet()));		
		broadsheetTable.getCellListeners().add(getBroadsheetTableCellAdapter(subLevels));

		//if(Boolean.TRUE.equals(isBroadsheetColumnTitleRotated()))
		//	broadsheetTable.setUpdateStyleClass("broadsheetTableStyleClass");

		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		broadsheetTable.postConstruct();
	}
	/*
	protected Boolean isBroadsheetColumnTitleRotated(){
		return subLevels!=null && subLevels.size()>6;
	}*/
	protected abstract Class<LEVEL> getLevelClass();
	protected abstract Class<LEVEL_OUTPUT> getLevelOutputClass();
	protected abstract Class<SUB_LEVEL> getSubLevelClass();
	protected abstract Class<SUB_LEVEL_OUTPUT> getSubLevelOutputClass();
	protected abstract Class<DETAIL> getDetailClass();
	protected abstract Class<DETAIL_OUTPUT> getDetailOutputClass();
	protected abstract Class<RESULT> getResultClass();
	protected abstract Class<RESULT_OUTPUT> getResultOutputClass();
	protected abstract ClassroomSession getClassroomSession();
	
	protected abstract Collection<SUB_LEVEL> getSubLevels();
	protected abstract Collection<RESULT> getResults();
	
	protected abstract Set<String> getResultTableSimpleFieldNameSet();
	protected abstract Set<String> getResultTableBroadsheetFieldNameSet();
	
	protected abstract CellAdapter<SUB_LEVEL, DETAIL, RESULT_OUTPUT> getBroadsheetTableCellAdapter(List<SUB_LEVEL> subLevels);
	
	protected DetailsConfigurationListener.Table.Adapter<RESULT,RESULT_OUTPUT> getResultTableCreationListener(){
		return new DetailsConfigurationListener.Table.Adapter<RESULT,RESULT_OUTPUT>(getResultClass(), getResultOutputClass()){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<RESULT> getIdentifiables() {
				return getResults();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE,Crud.DELETE};
			}
		};
	}
	
	protected org.cyk.ui.api.model.table.ColumnAdapter getResultTableColumnAdapter(){
		return new org.cyk.ui.api.model.table.ColumnAdapter(){
			private static final long serialVersionUID = 1L;

			@Override
			public Boolean isColumn(Field field) {
				return getResultTableSimpleFieldNameSet().contains(field.getName());
			}
		};
	}
	
	protected DetailsConfigurationListener.Table.Adapter<SUB_LEVEL,SUB_LEVEL_OUTPUT> getSubLevelTableCreationListener(){
		return new DetailsConfigurationListener.Table.Adapter<SUB_LEVEL,SUB_LEVEL_OUTPUT>(getSubLevelClass(), getSubLevelOutputClass()){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<SUB_LEVEL> getIdentifiables() {
				return getSubLevels();
			}
			
		};
	}
	
	@Getter @Setter @AllArgsConstructor
	public static class ColumnAdapter<SUB_LEVEL> extends org.cyk.ui.api.model.table.ColumnAdapter implements Serializable {
		private static final long serialVersionUID = 5857490517889645397L;
		
		private AbstractUserSession<?,?> userSession;
		private Teacher teacher;
		private Boolean isCoordinator;
		private Class<SUB_LEVEL> subLevelClass;
		private List<SUB_LEVEL> subLevels;
		private Integer numberOfColumnBeforeLevels;
		private Set<String> fieldNames;
		
		@Override
		public Boolean isColumn(Field field) {
			if(fieldNames.contains(field.getName())){
				if(AbstractStudentResultsOutputDetails.isDetailAverageFieldName(field.getName()))
					return subLevels!=null && AbstractStudentResultsOutputDetails.getDetailAverageFieldNameIndex(field.getName()) < subLevels.size();
				else{
					if(Boolean.TRUE.equals(userSession.getIsManager()) || isCoordinator)
						return Boolean.TRUE;
					else{
						if(teacher==null)
							return Boolean.FALSE;
						else
							return !ArrayUtils.contains(new String[]{AbstractStudentResultsOutputDetails.FIELD_EVALUATION_AVERAGE_DIVIDEND,
									AbstractStudentResultsOutputDetails.FIELD_EVALUATION_AVERAGE_DIVISOR,AbstractStudentResultsOutputDetails.FIELD_EVALUATION_AVERAGE_VALUE
										,AbstractStudentResultsOutputDetails.FIELD_EVALUATION_RANK_VALUE}, field.getName());
					}
				}
			}else
				return Boolean.FALSE;
		}
		
		@Override
		public void added(Column column) {
			super.added(column);
			if(AbstractStudentResultsOutputDetails.isDetailAverageFieldName(column.getField().getName())){
				column.setTitle(getDetailAverageColumnName(column));
			}else if(AbstractStudentResultsOutputDetails.FIELD_EVALUATION_AVERAGE_DIVISOR.equals(column.getField().getName())){
				column.setTitle(inject(LanguageBusiness.class).findText("count.of",new Object[]{ inject(LanguageBusiness.class).findClassLabelText(subLevelClass) }));
			}
		}
		
		protected String getDetailAverageColumnName(Column column){
			return inject(FormatterBusiness.class).format(subLevels.get(column.getIndex().intValue()-numberOfColumnBeforeLevels));
		}
		
	}
	
	@Getter @Setter @AllArgsConstructor
	public static abstract class CellAdapter<SUB_LEVEL,DETAIL extends AbstractStudentResult<?, ?>,RESULT_OUTPUT extends AbstractStudentResultsOutputDetails<?,?,?>> extends org.cyk.ui.api.model.table.CellAdapter<RESULT_OUTPUT> implements Serializable{
		private static final long serialVersionUID = -1462758931814509111L;
		
		protected Integer numberOfColumnBeforeLevels;
		protected List<SUB_LEVEL> subLevels;
		
		protected abstract NodeResults getNodeResults(SUB_LEVEL level);
		protected abstract SUB_LEVEL getSubLevel(DETAIL detail);
		protected abstract Collection<DETAIL> getDetailCollection();
		
		@Override
		public void added(Row<RESULT_OUTPUT> row, Column column, Cell cell) {
			super.added(row, column, cell);
			if(column.getIndex() < subLevels.size()+numberOfColumnBeforeLevels){
				if(row.getData().getMaster()==null){
					if(column.getIndex() >= numberOfColumnBeforeLevels && column.getIndex() < subLevels.size()+numberOfColumnBeforeLevels){
						SUB_LEVEL subLevel = subLevels.get(column.getIndex().intValue()-numberOfColumnBeforeLevels);
						NodeResults results = getNodeResults(subLevel);
						if(row.getData().getIdentifier().equals(AbstractOutputDetails.IDENTIFIER_1)){
							if(results.getNumberOfStudent()!=null && results.getNumberOfStudent()>0)
								cell.setValue(inject(NumberBusiness.class).format(results.getAverage()));
						}else if(row.getData().getIdentifier().equals(AbstractOutputDetails.IDENTIFIER_2)){
							if(results.getNumberOfStudent()!=null && results.getNumberOfStudent()>0)
								cell.setValue(results.getNumberOfStudentPassingEvaluationAverage()+Constant.CHARACTER_SLASH.toString()+
									results.getNumberOfStudent());
						}else if(row.getData().getIdentifier().equals(AbstractOutputDetails.IDENTIFIER_3)){
							if(results.getNumberOfStudentPassingEvaluationAverage()!=null && results.getNumberOfStudent()!=null && results.getNumberOfStudent()>0){
								NumberBusiness.FormatArguments formatArguments = new NumberBusiness.FormatArguments();
								formatArguments.setIsPercentage(Boolean.TRUE);
								formatArguments.setPercentageSymbol(null);
								BigDecimal percentage = new BigDecimal(results.getNumberOfStudentPassingEvaluationAverage()).
										divide(new BigDecimal(results.getNumberOfStudent()),4,RoundingMode.HALF_DOWN);
								cell.setValue(inject(NumberBusiness.class).format(percentage,formatArguments));
							}
						}
					}
				}else{
					DETAIL detail = null;
					final List<DETAIL> details = new ArrayList<>(getDetailCollection());
					for(DETAIL index : details){
						if(index.getStudent().equals(row.getData().getMaster().getStudent()) ){
							if(subLevels.indexOf(getSubLevel(index)) + numberOfColumnBeforeLevels == column.getIndex()){
								detail = index;
								break;
							}
						}
					}
					if(detail!=null){
						cell.setValue(inject(NumberBusiness.class).format(detail.getResults().getEvaluationSort().getAverage().getValue()));
					}
				}
			}
		}
		
	}
}
