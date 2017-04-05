package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.integration.enterpriseresourceplanning.AbstractEnterpriseResourcePlanningBusinessIT;
import org.cyk.system.school.model.SchoolConstant;

public abstract class AbstractIesaBusinessIT extends AbstractEnterpriseResourcePlanningBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer; 
     
    protected void installApplication(Boolean fake){	
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				/*if(ArrayUtils.contains(new String[]{CompanyConstant.Code.File.DOCUMENT_HEADER}, instanceCode)){
					if(PersistDataListener.RELATIVE_PATH.equals(name))
						return (T) "/report/iesa/salecashregistermovementlogo.png";
				}*/
				if(ArrayUtils.contains(new String[]{CompanyConstant.Code.File.DOCUMENT_HEADER}, instanceCode)){
					if(PersistDataListener.RELATIVE_PATH.equals(name)){
						return (T) "/report/iesa/document_header.png";
					}else if(PersistDataListener.BASE_PACKAGE.equals(name))
						return (T) SchoolBusinessLayer.class.getPackage();
				}
				if(ArrayUtils.contains(new String[]{CompanyConstant.Code.File.DOCUMENT_BACKGROUND}, instanceCode)){
					if(PersistDataListener.RELATIVE_PATH.equals(name))
						return (T) "/report/iesa/studentclassroomsessiondivisionreport_background.jpg";
					else if(PersistDataListener.BASE_PACKAGE.equals(name))
						return (T) SchoolBusinessLayer.class.getPackage();
				}
				if(ArrayUtils.contains(new String[]{CompanyConstant.Code.File.DOCUMENT_BACKGROUND_DRAFT}, instanceCode)){
					if(PersistDataListener.RELATIVE_PATH.equals(name))
						return (T) "/report/iesa/studentclassroomsessiondivisionreport_background.jpg";
					else if(PersistDataListener.BASE_PACKAGE.equals(name))
						return (T) SchoolBusinessLayer.class.getPackage();
				}
				return super.processPropertyValue(aClass, instanceCode, name, value);
			}
		});
    	
    	super.installApplication(fake);
    	
    	StudentBusinessImpl.Listener.Adapter listener = new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning();
    	listener.setCodePrefix("IESA");
    	StudentBusinessImpl.Listener.COLLECTION.add(listener);
    	
    	CompanyBusinessLayer.getInstance().enableEnterpriseResourcePlanning();
		
    	AbstractCompanyReportProducer.Listener.COLLECTION.add(new AbstractCompanyReportProducer.Listener.Adapter.Default(){
			private static final long serialVersionUID = 215473098986115952L;
			
			@Override
			public String[] getCustomerPersonRelationshipTypeCodes(AbstractIdentifiable identifiable) {
				return new String[]{RootConstant.Code.PersonRelationshipType.FAMILY_FATHER,RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER};
			}
			
			@Override
			public String getCustomerLabel(AbstractIdentifiable identifiable) {
				return "Parent";
			}
		});
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
