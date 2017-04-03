package org.cyk.system.school.model.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.utility.common.generator.RandomDataProvider;
import org.cyk.utility.common.model.table.AbstractCrossedDataReport;

@Getter @Setter
public class BroadsheetReport extends AbstractCrossedDataReport<BroadsheetReport,BroadsheetReport.Cell> implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/**/
	
	@Override
	protected String[] getGenerateRows() {
		String[] rows = new String[40];
		for(int i =0;i<rows.length;i++)
			rows[i] = RandomDataProvider.getInstance().getMale().lastName()
				+","+RandomDataProvider.getInstance().getMale().middleName()+" "+RandomDataProvider.getInstance().getMale().firstName();
		return rows;
	}
	
	@Override
	protected String[] getGenerateColumns() {
		return new String[]{"Mathematics","Sciences","History","Geography","French","English","Litterature","Biology"
				,"German","Foreign language","Social studies","physics","Business","Management","Culture","Music"
				,"Art craft","Design","Physical education","Knowledge","Vison","TOTAL","NUMBER OF SUBJECT","AVERAGE","RANK"};
	}
	
	/**/
	
	@Getter @Setter
	public static class Cell extends AbstractCrossedDataReport.AbstractCell<Cell> implements Serializable {
		private static final long serialVersionUID = 1L;
		
		protected String rank;
		
	}
	
}
