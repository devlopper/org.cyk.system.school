package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyPickList;

@Getter @Setter
public abstract class AbstractStudentClassroomSessionDivisionQueryManyFormModel<STUDENTSESSION extends AbstractIdentifiable> extends AbstractQueryManyFormModel.Default<STUDENTSESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice @InputManyChoice @InputManyPickList private List<PersonRelationshipType> personRelationshipTypes;
	
	/**/
	
	@Getter @Setter
	public static class AbstractStudentClassroomSessionDivisionSelectManyPageAdapter<STUDENTSESSION extends AbstractIdentifiable> extends AbstractSelectManyPage.Listener.Adapter.Default<STUDENTSESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractStudentClassroomSessionDivisionSelectManyPageAdapter(Class<STUDENTSESSION> aClass) {
			super(aClass);
		}
		
	}
}