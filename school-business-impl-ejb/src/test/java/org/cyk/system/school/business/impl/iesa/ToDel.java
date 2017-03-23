package org.cyk.system.school.business.impl.iesa;

import java.io.File;
import java.io.FileInputStream;

import org.apache.commons.io.IOUtils;
import org.cyk.utility.common.file.ExcelSheetReader;

public class ToDel {

	public static void main(String[] args) throws Exception {
		File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File excelWorkbookFile = new File(directory, "2016_2017_Trimester_1.xlsx");
		
		ExcelSheetReader excelSheetReader = new ExcelSheetReader.Adapter.Default(new File(directory, "2016_2017_Trimester_1.xlsx")){
			private static final long serialVersionUID = 1L;
    		@Override
    		public Boolean isRowAddable(Dimension.Row<String> row) {
    			System.out.println("ToDel.main(...).new Default() {...}.isRowAddable() : "+row);
    			return super.isRowAddable(row);
    		}
    	};
    	excelSheetReader.setWorkbookBytes(IOUtils.toByteArray(new FileInputStream(excelWorkbookFile)));
    	excelSheetReader.setIndex(0);
    	excelSheetReader.setFromRowIndex(1);
    	excelSheetReader.setFromColumnIndex(0);
    	//excelSheetReader.setRowCount(2);
    	
    	excelSheetReader.execute();
	}

}
