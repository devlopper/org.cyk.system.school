package org.cyk.system.school.ui.web.primefaces.page.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.api.model.pattern.tree.AbstractDataTreeForm;
import org.cyk.ui.web.primefaces.page.geography.AbstractDataTreeNodeEditPage;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelGroupEditPage extends AbstractDataTreeNodeEditPage<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Getter @Setter 
	@FieldOverrides(value = {
			@FieldOverride(name=AbstractDataTreeForm.FIELD_PARENT,type=LevelGroup.class)
			,@FieldOverride(name=AbstractDataTreeForm.FIELD_TYPE,type=LevelGroupType.class)
			})
	public static class Form extends AbstractDataTreeForm<LevelGroup,LevelGroupType> implements Serializable{
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
