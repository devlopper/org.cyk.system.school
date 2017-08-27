package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.globalidentification.GlobalIdentifierBusinessImpl;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.utility.common.helper.StringHelper;
import org.cyk.utility.common.helper.SystemHelper;

public class Iesa2016_2017_To_2017_2018Database extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
     
    @Override
    protected void businesses() {    	
    	GlobalIdentifierBusinessImpl.BuilderOneDimensionArray.IMAGE_DIRECTORY_PATH = StringHelper.getInstance().appendIfDoesNotEndWith(SystemHelper.getInstance()
    			.getProperty("images.directory.path"),"\\");
    	Iesa_2016_2017_DataSet dataSet = new Iesa_2016_2017_DataSet(); 	
    	dataSet.getIdentifiableCountByTransactionMap().put(StudentClassroomSession.class, 1);
    	dataSet.instanciate();
    	dataSet.save();
    	System.exit(0);
    }
         
}
