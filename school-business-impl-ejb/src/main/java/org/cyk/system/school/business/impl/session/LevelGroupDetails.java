package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;

@Getter @Setter
public class LevelGroupDetails extends AbstractOutputDetails<LevelGroup> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@IncludeInputs private CommonNodeInformationsDetails nodeInformations;
	
	public LevelGroupDetails(LevelGroup levelGroup) {
		super(levelGroup);
	}
	
	@Override
	public void setMaster(LevelGroup levelGroup) {
		super.setMaster(levelGroup);
		if(levelGroup!=null){
			if(nodeInformations==null)
				nodeInformations = new CommonNodeInformationsDetails(levelGroup.getNodeInformations());
			else
				nodeInformations.setMaster(levelGroup.getNodeInformations());
		}
	}
	
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
}