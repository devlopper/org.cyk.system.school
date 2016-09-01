package org.cyk.system.school.business.impl.integration;
import java.io.Serializable;

import javax.inject.Inject;

import lombok.Getter;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper;

@Getter
public abstract class AbstractSchoolFakedDataProducer extends AbstractFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Inject protected SchoolDataProducerHelper schoolDataProducerHelper;
	
	@Override
	protected Package getBasePackage() {
		return SchoolBusinessLayer.class.getPackage();
	}
	
	
		
}