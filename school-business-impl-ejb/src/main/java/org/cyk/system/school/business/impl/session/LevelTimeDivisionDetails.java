package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class LevelTimeDivisionDetails extends AbstractOutputDetails<LevelTimeDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String level,timeDivisionType,index;
		
	public LevelTimeDivisionDetails(LevelTimeDivision levelTimeDivision) {
		super(levelTimeDivision);
		level = formatUsingBusiness(levelTimeDivision.getLevel());
		timeDivisionType = formatUsingBusiness(levelTimeDivision.getTimeDivisionType());
		index = levelTimeDivision.getIndex()+"";
	}
	
	public static final String FIELD_LEVEL = "level";
	public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	public static final String FIELD_INDEX = "index";
}