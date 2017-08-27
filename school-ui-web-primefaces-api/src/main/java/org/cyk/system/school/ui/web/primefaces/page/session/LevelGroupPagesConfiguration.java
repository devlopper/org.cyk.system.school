package org.cyk.system.school.ui.web.primefaces.page.session;

import java.io.Serializable;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.ui.web.primefaces.CommonNodeInformationsFormModel;
import org.cyk.ui.web.primefaces.page.pattern.tree.AbstractDataTreePagesConfiguration;
import org.cyk.utility.common.Constant.Action;
import org.cyk.utility.common.helper.FieldHelper;

public class LevelGroupPagesConfiguration extends AbstractDataTreePagesConfiguration<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Override
	protected String[] getFieldNames(Action action) {
		String[] names = ArrayUtils.addAll(FieldHelper.getInstance().getNamesWhereReferencedByStaticField(CommonNodeInformationsFormModel.class).toArray(new String[]{})
				,LevelGroupEditPage.Form.FIELD_NODE_INFORMATIONS);
		return ArrayUtils.addAll(super.getFieldNames(action),names);
	}
	
}
