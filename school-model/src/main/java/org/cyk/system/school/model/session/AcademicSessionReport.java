package org.cyk.system.school.model.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.model.structure.CompanyReport;
import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter
public class AcademicSessionReport extends AbstractGeneratable<AcademicSessionReport> implements Serializable {

	private static final long serialVersionUID = 7967195187341455422L;
	
	private CompanyReport company = new CompanyReport();
	private String fromDateToDate;
	
	@Override
	public void generate() {
		fromDateToDate = "2014-2015";
		company.generate();
	}
	
}