package org.cyk.system.school.business.impl.integration.enterpriseresourceplanning;

import java.io.Serializable;

import javax.inject.Singleton;

import org.cyk.system.company.business.impl.AbstractCompanyFakedDataProducer;

import lombok.Getter;

@Singleton @Getter
public class EnterpriseResourcePlanningFakedDataProducer extends AbstractCompanyFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
	
	@Override
	protected void structure() {
		
	}

	@Override
	protected void doBusiness(Listener listener) {
		
	}

	
}
