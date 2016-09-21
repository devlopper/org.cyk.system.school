package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Student;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.ui.web.primefaces.page.party.AbstractActorQueryOneFormModel;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,type=Student.class)})
public class StudentQueryOneFormModel extends AbstractActorQueryOneFormModel<Student> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
		
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractActorSelectOnePageAdapter<Student> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(Student.class);
		}
		
		protected void initialiseSelect(AbstractSelectOnePage<?> selectPage){
			super.initialiseSelect(selectPage);
			//CompanyWebManager.getInstance().initialiseSelectClassroomSession(selectPage, AbstractQueryOneFormModel.FIELD_IDENTIFIABLE, null,null,null);
		}
		
		
	}
}