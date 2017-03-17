package org.cyk.system.school.business.impl.iesa;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void installApplication(Boolean fake) {
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
    }
    
    @Override
    protected void businesses() {
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport( ((IesaFakedDataProducer)getFakedDataProducer()).generate()
    			, new Boolean[]{Boolean.FALSE},Boolean.TRUE, Boolean.FALSE);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	dataProducer.getDivisionOrderNumbers().add(2l);
    	dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
