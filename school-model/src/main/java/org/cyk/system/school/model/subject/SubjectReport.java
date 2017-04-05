package org.cyk.system.school.model.subject;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;

@Getter @Setter
public class SubjectReport extends AbstractIdentifiableReport<Subject> implements Serializable {

	private static final long serialVersionUID = 5049082461268056567L;

}
