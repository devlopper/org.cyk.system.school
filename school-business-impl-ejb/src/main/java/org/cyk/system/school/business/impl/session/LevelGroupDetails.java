package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.LevelGroup;

public class LevelGroupDetails extends AbstractOutputDetails<LevelGroup> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
		
	public LevelGroupDetails(LevelGroup levelGroup) {
		super(levelGroup);
		
	}
	
	
}