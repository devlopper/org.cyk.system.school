package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.model.AbstractBusinessIdentifiedEditFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;

@Named @ViewScoped @Getter @Setter
public class LevelGroupEditPage extends AbstractCrudOnePage<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
			
	public static class Form extends AbstractBusinessIdentifiedEditFormModel<LevelGroup> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
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
