package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.DataSet;
import org.cyk.system.root.business.impl.globalidentification.GlobalIdentifierBusinessImpl;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.StringHelper;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;

public class Iesa_2016_2017_DataSet extends DataSet implements Serializable {

	private static final long serialVersionUID = 1L;

	public Iesa_2016_2017_DataSet() {
		super(SchoolBusinessLayer.class);
		
		setExcelWorkbookFileName("data\\iesa\\IESA_2017_2016.xlsx");
    	
    	addClass(GlobalIdentifier.class,new GlobalIdentifierBusinessImpl.BuilderOneDimensionArray(){
    		private static final long serialVersionUID = 1L;

			@Override
    		protected GlobalIdentifier __execute__() {
    			Object[] array = getInput();
    			if(!StringHelper.getInstance().isBlank((String)array[10])){
    				String date = (String)array[10];
    				String[] p = StringUtils.split(StringUtils.substringBefore(date, Constant.CHARACTER_SPACE.toString()),"-");
    				array[10] = p[2]+"/"+p[1]+"/"+p[0];
    			}
    			return super.__execute__();
    		}
    	});
    	
    	addClass(JobTitle.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<JobTitle>(JobTitle.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected JobTitle __execute__() {
    			JobTitle jobTitle = super.__execute__();
    			GlobalIdentifier globalIdentifier = InstanceHelper.Pool.getInstance().get(GlobalIdentifier.class, getInput()[0]);
    			jobTitle.setGlobalIdentifier(new InstanceHelper.Copy.Adapter.Default<GlobalIdentifier>(globalIdentifier).execute());
    			if(jobTitle.getIdentifier()==null)
    				jobTitle.getGlobalIdentifier().setIdentifier(null);
    			else{
    				jobTitle.getGlobalIdentifier().setIdentifiable(jobTitle);
    				jobTitle.getGlobalIdentifier().setIdentifier(InstanceHelper.getInstance().generateFieldValue(jobTitle.getGlobalIdentifier(), GlobalIdentifier.FIELD_IDENTIFIER, String.class));
    			}
    			return jobTitle;
    		}
    	});
    	
    	instanceKeyBuilderMap.put(JobTitle.class, new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			return new ArrayHelper.Dimension.Key(InstanceHelper.Pool.getInstance().get(GlobalIdentifier.class, getInput()[0]).getCode());
    		}
    	});
	}

	
	
}
