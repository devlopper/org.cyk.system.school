package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionSubjectCreateManyPage.Form;
import org.cyk.ui.api.model.AbstractQueryFormModel;
import org.cyk.ui.web.api.AjaxListener.ListenValueMethod;
import org.cyk.ui.web.primefaces.page.AbstractBusinessEntityFormOnePage;
import org.cyk.ui.web.primefaces.page.SelectPage;
import org.cyk.ui.web.primefaces.page.SelectPageListener;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;
import org.cyk.utility.common.cdi.AbstractBean;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivisionSubject.class)})
public class ClassroomSessionDivisionSubjectQueryFormModel extends AbstractQueryFormModel.Default<ClassroomSessionDivisionSubject> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSessionDivision classroomSessionDivision;
	//@Inject @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	@Override @Sequence(direction=Direction.AFTER,field=CLASSROOM_SESSION_DIVISION)
	public ClassroomSessionDivisionSubject getIdentifiable() {
		return super.getIdentifiable();
	}
	
	public static final String CLASSROOM_SESSION = "classroomSession";
	public static final String CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends SelectPageListener.Adapter.Default<ClassroomSessionDivisionSubject,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivisionSubject.class);
			//type = SelectPageListener.Type.IDENTIFIER;
		}
		
		@Override
		public void afterInitialisationEnded(AbstractBean bean) {
			super.afterInitialisationEnded(bean);
			final SelectPage selectPage = (SelectPage) bean;
			SchoolWebManager.getInstance().initialiseSelectClassroomSessionDivisionSubject(selectPage, Form.CLASSROOM_SESSION, Form.CLASSROOM_SESSION_DIVISION
					, AbstractQueryFormModel.FIELD_IDENTIFIABLE);
			/*
			selectPage.setChoices(Form.CLASSROOM_SESSION, SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findByAcademicSession(
					SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null)));
			
			selectPage.createAjaxBuilder(Form.CLASSROOM_SESSION).updatedFieldNames(Form.CLASSROOM_SESSION_DIVISION,AbstractQueryFormModel.FIELD_IDENTIFIABLE)
			.method(ClassroomSession.class,new ListenValueMethod<ClassroomSession>() {
				@Override
				public void execute(ClassroomSession value) {
					selectClassroomSession(selectPage,value);
				}
			}).build();
			
			selectPage.createAjaxBuilder(Form.CLASSROOM_SESSION_DIVISION).updatedFieldNames(AbstractQueryFormModel.FIELD_IDENTIFIABLE)
			.method(ClassroomSessionDivision.class,new ListenValueMethod<ClassroomSessionDivision>() {
				@Override
				public void execute(ClassroomSessionDivision value) {
					selectClassroomSessionDivision(selectPage,value);
				}
			}).build();
			*/
		}
		/*
		public static void selectClassroomSession(AbstractBusinessEntityFormOnePage<?> page,ClassroomSession classroomSession){
			if(classroomSession==null)
				page.setChoices(Form.CLASSROOM_SESSION_DIVISION,null);
			else
				page.setChoices(Form.CLASSROOM_SESSION_DIVISION, SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession));
			
			selectClassroomSessionDivision(page,null);
		}
		public static void selectClassroomSessionDivision(AbstractBusinessEntityFormOnePage<?> page,ClassroomSessionDivision classroomSessionDivision){
			if(classroomSessionDivision==null){
				page.setChoices(AbstractQueryFormModel.FIELD_IDENTIFIABLE, null);
			}else{
				page.setChoices(AbstractQueryFormModel.FIELD_IDENTIFIABLE, SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness()
						.findByClassroomSessionDivision(classroomSessionDivision));
			}
		}*/
	}
}