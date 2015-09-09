package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractEnumeration;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.IntervalCollection;

@Entity
@Getter @Setter
public class LevelName extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;

	@OneToOne @JoinColumn(name="resultsReportFile")
	private File studentClassroomSessionDivisionResultsReportFile = new File();
	
 	private String studentClassroomSessionDivisionResultsReportHeadRight;
	
	@OneToOne private IntervalCollection studentSubjectAverageAppreciation = new IntervalCollection();
	@OneToOne private IntervalCollection studentClassroomSessionDivisionAverageAppreciation = new IntervalCollection();
	@OneToOne private IntervalCollection studentClassroomSessionAverageAppreciation = new IntervalCollection();
	
	public LevelName() {}

	public LevelName(String code,String name, String abbreviation,File studentClassroomSessionDivisionResultsReportFile) {
		super(code,name, abbreviation,null);
		this.studentClassroomSessionDivisionResultsReportFile = studentClassroomSessionDivisionResultsReportFile;
	}
	
}
