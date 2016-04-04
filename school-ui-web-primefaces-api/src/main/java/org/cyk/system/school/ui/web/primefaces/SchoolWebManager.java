package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Named;
import javax.inject.Singleton;

import lombok.Getter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Named @Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolWebManager.DEPLOYMENT_ORDER) @Getter
public class SchoolWebManager extends AbstractSchoolWebManager implements Serializable {

	public static final int DEPLOYMENT_ORDER = SchoolBusinessLayer.DEPLOYMENT_ORDER+1;
	private static final long serialVersionUID = 7231721191071228908L;

	private static SchoolWebManager INSTANCE;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation(); 
	}
		
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}

	
}
