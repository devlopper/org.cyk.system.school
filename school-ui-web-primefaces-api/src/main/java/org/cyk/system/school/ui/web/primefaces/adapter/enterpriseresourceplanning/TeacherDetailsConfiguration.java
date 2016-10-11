package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.business.impl.actor.TeacherDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.ui.web.primefaces.adapter.enterpriseresourceplanning.ActorDetailsConfiguration;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;

@Getter @Setter
public class TeacherDetailsConfiguration extends ActorDetailsConfiguration implements Serializable {

	private static final long serialVersionUID = 1L;
	
	public static final List<String> TEACHER_EXTENDED_FIELD_NAMES = new ArrayList<>();
	static{
		TEACHER_EXTENDED_FIELD_NAMES.addAll(ACTOR_EXTENDED_FIELD_NAMES);
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
			
		}
		
		@Override
		public String[] getFieldNames() {
			return TEACHER_EXTENDED_FIELD_NAMES.toArray(new String[]{});
		}
	}
	
	@Getter @Setter @NoArgsConstructor
	public static class DetailsControlSetAdapter extends ActorDetailsConfiguration.DetailsControlSetAdapter implements Serializable{
		
		private static final long serialVersionUID = 1L;
		
		public DetailsControlSetAdapter(Class<?> identifiableClass) {
			super(identifiableClass);
		}
		
		@Override
		public String[] getFieldNames() {
			return TEACHER_EXTENDED_FIELD_NAMES.toArray(new String[]{});
		}
				
		@Override
		public Boolean build(Object data, Field field) {
			if(data instanceof TeacherDetails && TEACHER_EXTENDED_FIELD_NAMES.contains(field.getName()))
				return Boolean.TRUE;
			return super.build(data, field);
		}
	}
}
