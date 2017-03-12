package org.cyk.system.school.business.impl.integration;
import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.company.business.impl.AbstractCompanyFakedDataProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper;

import lombok.Getter;

@Getter
public abstract class AbstractSchoolFakedDataProducer extends AbstractCompanyFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Inject protected SchoolDataProducerHelper schoolDataProducerHelper;
	
	@Override
	protected Package getBasePackage() {
		return SchoolBusinessLayer.class.getPackage();
	}
	
	
		
}