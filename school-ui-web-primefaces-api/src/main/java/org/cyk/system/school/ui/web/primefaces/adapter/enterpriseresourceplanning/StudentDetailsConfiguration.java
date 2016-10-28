package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.ui.api.model.party.AbstractPersonEditFormModel;
import org.cyk.ui.web.primefaces.adapter.enterpriseresourceplanning.ActorDetailsConfiguration;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;

@Getter @Setter
public class StudentDetailsConfiguration extends ActorDetailsConfiguration implements Serializable {

	private static final long serialVersionUID = 1L;
	
	public static final List<String> STUDENT_EXTENDED_FIELD_NAMES = new ArrayList<>();
	static{
		STUDENT_EXTENDED_FIELD_NAMES.addAll(ACTOR_EXTENDED_FIELD_NAMES);
		STUDENT_EXTENDED_FIELD_NAMES.add(StudentEditPage.Form.FIELD_ADMISSION_LEVEL_TIME_DIVISION);
		STUDENT_EXTENDED_FIELD_NAMES.add(StudentEditPage.Form.FIELD_CLASSROOMSESSION);
	}
	
	@SuppressWarnings("rawtypes")
	@Override
	public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
		return new DetailsControlSetAdapter(clazz);
	}
	
	/**/
	
	@Getter @Setter
	public static class FormControlSetAdapter extends ActorDetailsConfiguration.FormControlSetAdapter implements Serializable{
		
		private static final long serialVersionUID = 1L;
		
		public FormControlSetAdapter() {
			super(Student.class);
			//addFieldNamePairOrder(StudentEditPage.Form.FIELD_NATIONALITY, StudentEditPage.Form.FIELD_ADMISSION_LEVEL_TIME_DIVISION);
			//addFieldNamePairOrder(StudentEditPage.Form.FIELD_ADMISSION_LEVEL_TIME_DIVISION, StudentEditPage.Form.FIELD_CLASSROOMSESSION);
			/*if(Boolean.TRUE.equals(PersonBusiness.FindNamesArguments.FIRST_NAME_IS_FIRST))
				addFieldNamePairOrder(AbstractPersonEditFormModel.FIELD_NAME, AbstractPersonEditFormModel.FIELD_LAST_NAMES);
			else
				addFieldNamePairOrder(AbstractPersonEditFormModel.FIELD_LAST_NAMES, AbstractPersonEditFormModel.FIELD_NAME);
			*/
		}
		
		@Override
		public String[] getFieldNames() {
			return STUDENT_EXTENDED_FIELD_NAMES.toArray(new String[]{});
		}
		
		@Override
		public List<String> getExpectedFieldNames() {
			List<String> l = super.getExpectedFieldNames();
			for(String[] f : fieldNamePairOrders)
				System.out.println(StringUtils.join(f," , "));
			return l;
		}
	}
	
	@Getter @Setter @NoArgsConstructor
	public static class DetailsControlSetAdapter extends ActorDetailsConfiguration.DetailsControlSetAdapter implements Serializable{
		
		private static final long serialVersionUID = 1L;
		
		public DetailsControlSetAdapter(Class<?> identifiableClass) {
			super(identifiableClass);
			addFieldNamePairOrder(StudentDetails.FIELD_NATIONALITY, StudentDetails.FIELD_ADMISSION_LEVEL_TIME_DIVISION);
			addFieldNamePairOrder(StudentDetails.FIELD_ADMISSION_LEVEL_TIME_DIVISION, StudentDetails.FIELD_CLASSROOMSESSION);
			/*if(Boolean.TRUE.equals(PersonBusiness.FindNamesArguments.FIRST_NAME_IS_FIRST))
				addFieldNamePairOrder(AbstractPersonEditFormModel.FIELD_NAME, AbstractPersonEditFormModel.FIELD_LAST_NAMES);
			else
				addFieldNamePairOrder(AbstractPersonEditFormModel.FIELD_LAST_NAMES, AbstractPersonEditFormModel.FIELD_NAME);
			*/
		}
		
		@Override
		public String[] getFieldNames() {
			return STUDENT_EXTENDED_FIELD_NAMES.toArray(new String[]{});
		}
				
		@Override
		public Boolean build(Object data, Field field) {
			if(data instanceof StudentDetails && STUDENT_EXTENDED_FIELD_NAMES.contains(field.getName()))
				return Boolean.TRUE;
			return super.build(data, field);
		}
	}
}
