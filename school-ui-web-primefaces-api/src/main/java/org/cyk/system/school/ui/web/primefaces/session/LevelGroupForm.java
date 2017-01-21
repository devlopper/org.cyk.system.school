package org.cyk.system.school.ui.web.primefaces.session;

import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.ui.api.model.pattern.tree.AbstractDataTreeForm;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter 
@FieldOverrides(value = {
		@FieldOverride(name=AbstractDataTreeForm.FIELD_PARENT,type=LevelGroup.class)
		,@FieldOverride(name=AbstractDataTreeForm.FIELD_TYPE,type=LevelGroupType.class)
		})
public class LevelGroupForm extends AbstractDataTreeForm<LevelGroup,LevelGroupType> {

	private static final long serialVersionUID = -3927257570208213271L;

}
