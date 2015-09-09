package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Singleton;

import org.cyk.system.root.business.impl.AbstractTestHelper;

@Singleton
public class SchoolBusinessTestHelper extends AbstractTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	
	private static SchoolBusinessTestHelper INSTANCE;
		
	/**/
	
	public static SchoolBusinessTestHelper getInstance() {
		return INSTANCE;
	}
	
}
