package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.School;

public class SchoolDetails extends AbstractOutputDetails<School> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
		
	public SchoolDetails(School school) {
		super(school);
		
	}
	
	
}