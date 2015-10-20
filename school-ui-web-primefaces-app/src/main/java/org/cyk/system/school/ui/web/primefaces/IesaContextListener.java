package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.servlet.annotation.WebListener;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.ui.api.data.collector.form.FormConfiguration.Type;
import org.cyk.ui.api.model.party.DefaultActorEditFormModel;
import org.cyk.ui.api.model.party.DefaultActorReadFormModel;
import org.cyk.ui.api.model.party.DefaultPersonEditFormModel;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormManyPageListener;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormOnePageListener;

@WebListener
public class IesaContextListener extends AbstractSchoolContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormOnePageListener(Class<ACTOR> actorClass,BusinessEntityFormOnePageListener<?> listener) {
		super.registerBusinessEntityFormOnePageListener(actorClass, listener);
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_TITLE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_SURNAME
					,DefaultPersonEditFormModel.FIELD_BIRTH_DATE,DefaultPersonEditFormModel.FIELD_BIRTH_LOCATION
					,DefaultPersonEditFormModel.FIELD_SEX,DefaultPersonEditFormModel.FIELD_IMAGE);
		}
	}
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormManyPageListener(Class<ACTOR> actorClass,BusinessEntityFormManyPageListener<?> listener) {
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}
		super.registerBusinessEntityFormManyPageListener(actorClass, listener);
	}
	
}
