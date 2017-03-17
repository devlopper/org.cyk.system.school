package org.cyk.system.school.model.session;

import java.io.Serializable;

import org.cyk.system.company.model.structure.CompanyReport;
import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class AcademicSessionReport extends AbstractIdentifiableReport<AcademicSessionReport> implements Serializable {

	private static final long serialVersionUID = 7967195187341455422L;
	
	//TODO to be replaced by school report
	private CompanyReport company = new CompanyReport();
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source==null){
			
		}else{
			//this.company.setSource((((AcademicSession)source).getcom); 
		}
	}
	
	@Override
	public void generate() {
		company.generate();
	}
	
}