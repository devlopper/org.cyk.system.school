package org.cyk.system.school.business.impl.integration;
import java.io.Serializable;

import lombok.Getter;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;

@Getter
public abstract class AbstractSchoolFakedDataProducer extends AbstractFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	protected SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
	
	@Override
	protected void initialisation() {
		super.initialisation();
		rootRandomDataProvider.getRandomDataProviderListeners().add(new RootRandomDataProvider.RootRandomDataProviderAdapter(){
			private static final long serialVersionUID = -4292999908835323092L;

			@Override
			public void set(Object object) {
				super.set(object);
				
			}
		});
	}
	
	@Override
	protected Package getBasePackage() {
		return SchoolBusinessLayer.class.getPackage();
	}
		
}