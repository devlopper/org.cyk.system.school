package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.model.AbstractBusinessIdentifiedEditFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionEditPage extends AbstractCrudOnePage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
	}
		
	public static class Form extends AbstractBusinessIdentifiedEditFormModel<AcademicSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		//@Input @InputChoice @InputManyChoice @InputManyPickList private List<LevelGroup> levelGroups;
		//@Input @InputChoice @InputManyChoice @InputManyPickList private List<LevelName> levelNames;
		
		@IncludeInputs private CommonNodeInformationsFormModel nodeInformations = new CommonNodeInformationsFormModel();
		
		@Override
		public void read() {
			super.read();
			nodeInformations.set(identifiable.getNodeInformations());
		}
		
		@Override
		public void write() {
			super.write();
			nodeInformations.write(identifiable.getNodeInformations());
		}
		
		public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
		
		
	}

}
