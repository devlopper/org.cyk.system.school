package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.model.SelectItem;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.geography.ContactBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.party.person.PersonRelationshipBusiness;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.geography.ElectronicMail;
import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonRelationship;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.session.AbstractStudentClassroomSessionDivisionQueryManyFormModel;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.data.collector.form.ControlSet;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.api.model.table.Cell;
import org.cyk.ui.api.model.table.CellAdapter;
import org.cyk.ui.api.model.table.Column;
import org.cyk.ui.api.model.table.ColumnAdapter;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.builder.UrlStringBuilder;
import org.cyk.utility.common.cdi.AbstractBean;
import org.omnifaces.util.Faces;
import org.primefaces.extensions.model.dynaform.DynaFormControl;
import org.primefaces.extensions.model.dynaform.DynaFormLabel;
import org.primefaces.extensions.model.dynaform.DynaFormModel;
import org.primefaces.extensions.model.dynaform.DynaFormRow;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryManyFormModel.FIELD_IDENTIFIABLES,type=StudentClassroomSessionDivision.class)})
public class StudentClassroomSessionDivisionQueryManyFormModel extends AbstractStudentClassroomSessionDivisionQueryManyFormModel<StudentClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	public static final String FIELD_PERSON_RELATIONSHIP_TYPES = "personRelationshipTypes";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractStudentClassroomSessionDivisionSelectManyPageAdapter<StudentClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(StudentClassroomSessionDivision.class);
		}
		
		@Override
		public void initialisationEnded(AbstractBean bean) {
			super.initialisationEnded(bean);
		}
				
		@Override
		public Collection<StudentClassroomSessionDivision> getIdentifiables(AbstractSelectManyPage<?> page) {
			AcademicSession academicSession = inject(AcademicSessionBusiness.class).findCurrent(null);
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(page.getActionIdentifier())){
				return inject(StudentClassroomSessionDivisionBusiness.class).findByAcademicSessionByClassroomSessionDivisionOrderNumber(
						academicSession,academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
			}else
				return inject(StudentClassroomSessionDivisionBusiness.class).findByAcademicSessionByClassroomSessionDivisionOrderNumber(
						academicSession,academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
		}
		
		@Override
		public UrlStringBuilder getRedirectUrlStringBuilder(AbstractSelectManyPage<?> selectManyPage, Object data,String actionIdentifier) {
			UrlStringBuilder urlStringBuilder = super.getRedirectUrlStringBuilder(selectManyPage, data, actionIdentifier);
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				urlStringBuilder.getQueryStringBuilder().addParameter(inject(UIManager.class).businessEntityInfos(PersonRelationshipType.class).getIdentifier()
					,inject(WebManager.class).encodeIdentifiablesAsRequestParameterValue(((StudentClassroomSessionDivisionQueryManyFormModel)data)
					.getPersonRelationshipTypes())).addParameter(UniformResourceLocatorParameter.ENCODED,inject(UIManager.class)
							.businessEntityInfos(PersonRelationshipType.class).getIdentifier());
			}
			return urlStringBuilder;
		}
	}
	
	@Getter @Setter
	public static class ProcessPageAdapter extends AbstractProcessManyPage.Listener.Adapter.Default<StudentClassroomSessionDivision,Long> implements Serializable {

		private static final long serialVersionUID = -8606970206843948983L;

		public ProcessPageAdapter() {
			super(StudentClassroomSessionDivision.class);
		}
		
		private Collection<PersonRelationshipType> getPersonRelationshipTypes(){
			return inject(WebManager.class).decodeIdentifiablesRequestParameter(PersonRelationshipType.class
					, inject(UIManager.class).businessEntityInfos(PersonRelationshipType.class).getIdentifier(), Faces.getRequest());
		}
		
		@Override
		protected void initialiseProcessOnInitialisationEnded(final AbstractProcessManyPage<?> page) {
			super.initialiseProcessOnInitialisationEnded(page);
			SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
			page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
			if(ArrayUtils.contains(new String[]{schoolBusinessLayer.getActionSendStudentClassroomSessionDivisionReportFiles()}, page.getActionIdentifier())){
				Collection<Person> studentPersons = new ArrayList<>();
				for(StudentClassroomSessionDivision studentClassroomSessionDivision : commonUtils.castCollection(page.getElements(), StudentClassroomSessionDivision.class))
					studentPersons.add(studentClassroomSessionDivision.getStudent().getPerson());				
				final List<PersonRelationshipType> personRelationshipTypes = (List<PersonRelationshipType>) getPersonRelationshipTypes();
				final Collection<PersonRelationship> personRelationships = inject(PersonRelationshipBusiness.class).findByPerson2ByTypes(studentPersons,personRelationshipTypes);
				//Collection<Person> parentPersons = inject(PersonBusiness.class).findByPersonRelationshipPerson2ByPersonRelationshipTypes(studentPersons,personRelationshipTypes);
				//Collection<Person> person1s = inject(PersonBusiness.class).getPerson1(personRelationships);
				inject(ContactBusiness.class).findByCollectionsByClass(inject(PersonBusiness.class).getContactCollections(inject(PersonBusiness.class)
						.getPerson1(personRelationships)),ElectronicMail.class);
				
				page.getForm().getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
				page.getForm().getControlSetListeners().add(new ControlSetAdapter<Object>(){
					private static final long serialVersionUID = 1L;
					@Override
					public String fiedLabel(ControlSet<Object, DynaFormModel, DynaFormRow, DynaFormLabel, DynaFormControl, SelectItem> controlSet,Object data, Field field) {
						if(AbstractOutputDetails.isExtendedFieldName(field.getName()))
							return personRelationshipTypes.get(AbstractOutputDetails.getExtendedFieldNameIndex(field.getName())).getName();
						return super.fiedLabel(controlSet, data, field);
					}
				});
				
				page.getTable().getColumnListeners().add(new ColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public void added(Column column) {
						super.added(column);
						if(AbstractOutputDetails.isExtendedFieldName(column.getField().getName())){
							column.setTitle(personRelationshipTypes.get(AbstractOutputDetails.getExtendedFieldNameIndex(column.getField().getName())).getName());
						}
					}
				});
				
				page.getTable().getCellListeners().add(new CellAdapter<Object>(){
					@Override
					public void added(Row<Object> row, Column column, Cell cell) {
						super.added(row, column, cell);
						if(AbstractOutputDetails.isExtendedFieldName(column.getField().getName())){
							PersonRelationshipType personRelationshipType = personRelationshipTypes.get(AbstractOutputDetails.getExtendedFieldNameIndex(column.getField().getName()));
							StudentClassroomSessionDivision studentClassroomSessionDivision = (StudentClassroomSessionDivision) ((Item)row.getData()).getMaster();
							for(PersonRelationship personRelationship : personRelationships){
								if(personRelationship.getType().equals(personRelationshipType) 
										&& personRelationship.getPerson2().equals(studentClassroomSessionDivision.getStudent().getPerson())){
									cell.setValue(commonUtils.concatenate(cell.getValue(), new String[]{inject(PersonBusiness.class).findNames(personRelationship.getPerson1())
											+Constant.CHARACTER_LEFT_PARENTHESIS+StringUtils.join(personRelationship.getPerson1().getContactCollection().getElectronicMails()
													,Constant.CHARACTER_COMA.toString())+Constant.CHARACTER_RIGHT_PARENTHESIS}
										, Constant.CHARACTER_COMA.toString()));
								}
							}
						}
					}
				});
			}
		}
		
		@Override
		public void serve(AbstractProcessManyPage<?> page,Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				inject(StudentClassroomSessionDivisionBusiness.class).sendReportFileToEmail(commonUtils.castCollection(page.getElements(), StudentClassroomSessionDivision.class));
			}
		}
		
		@Override
		public Class<?> getFormDataClass(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier))
				return SendMessageForm.class;
			return Form.class;
		}
		
		@Override
		public Boolean getShowForm(AbstractProcessManyPage<?> processManyPage,String actionIdentifier) {
			return ArrayUtils.contains(new String[]{SchoolBusinessLayer.getInstance().getActionSendStudentClassroomSessionDivisionReportFiles()}, actionIdentifier);
		}
		
		@Override
		public Class<?> getItemClass(AbstractProcessManyPage<?> page) {
			return Item.class;
		}
		
		@Override
		public Boolean isItemTableColumn(AbstractProcessManyPage<?> processManyPage, Field field) {
			if(AbstractOutputDetails.isExtendedFieldName(field.getName())){
				return AbstractOutputDetails.getExtendedFieldNameIndex(field.getName()) < getPersonRelationshipTypes().size();
			}
			
			return ArrayUtils.contains(new String[]{Item.FIELD_NAME}, field.getName());
		}
		
		@Getter @Setter
		public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			
		}
		
		@Getter @Setter
		public static class SendMessageForm extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
			private static final long serialVersionUID = -4741435164709063863L;
			
			
			
		}
		
		@Getter @Setter
		public static class Item extends AbstractProcessManyPage.ProcessItem implements Serializable{
			
			private static final long serialVersionUID = -4741435164709063863L;
			
			public Item(AbstractIdentifiable identifiable) {
				super(identifiable);
			}
			
			
		}
	}
}