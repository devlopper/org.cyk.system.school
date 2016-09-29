package org.cyk.system.school.ui.web.primefaces.iesa;

import java.io.Serializable;
import java.lang.reflect.Field;

import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.ui.web.primefaces.iesa.ContextListener.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.Table.ColumnAdapter;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.DetailsConfiguration;

public class PrimefacesManager extends org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager implements Serializable {

	private static final long serialVersionUID = -8716834916609095637L;
	
	@Override
	public SystemMenu getSystemMenu(UserSession userSession) {
		return SystemMenuBuilder.getInstance().build(userSession);
	}
	
	@Override
	protected void configureStudentClassroomSessionDivisionSubjectClass() {
		super.configureStudentClassroomSessionDivisionSubjectClass();
		registerDetailsConfiguration(/*org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectDetails.class*/AbstractStudentClassroomSessionDivisionSubjectDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						//if(data instanceof StudentClassroomSessionDetails)
							return isFieldNameIn(field,StudentClassroomSessionDivisionSubjectDetails.FIELD_STUDENT
									,StudentClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT);
						
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,StudentClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
								,StudentClassroomSessionDivisionSubjectDetails.FIELD_TEST1,StudentClassroomSessionDivisionSubjectDetails.FIELD_TEST2
								,StudentClassroomSessionDivisionSubjectDetails.FIELD_EXAM);
					}
				};
			}
		});
	}
	
}
