package org.cyk.system.school.model.actor;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.party.person.AbstractActorReportTemplateFile;

@Getter @Setter @NoArgsConstructor
public class StudentReportTemplateFile extends AbstractActorReportTemplateFile<StudentReportTemplateFile,StudentReport> implements Serializable {

	private static final long serialVersionUID = 7332510774063666925L;

}
