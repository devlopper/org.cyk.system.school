package org.cyk.system.school.model.session;

import java.io.Serializable;

import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionReportTemplateFile extends AbstractReportTemplateFile<ClassroomSessionDivisionReportTemplateFile> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	@Override
	public void generate() {
		super.generate();
		classroomSessionDivision.generate();
	}
	
}
