package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.model.time.AbstractIdentifiablePeriod;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private AcademicSession academicSession;
	@ManyToOne @NotNull private LevelTimeDivision levelTimeDivision;
	private String suffix;
	@ManyToOne private Teacher coordinator;
	@Embedded private NodeResults results = new NodeResults();
	
	@Column(nullable=false) @NotNull private Long numberOfDivisions=0l;
	
	@Transient private Collection<ClassroomSessionDivision> divisions = new ArrayList<>();
	
	public ClassroomSession(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Teacher coordinator) {
		super();
		this.academicSession = academicSession;
		this.levelTimeDivision = levelTimeDivision;
		this.coordinator = coordinator;
	}
	
	public NodeResults getResults(){
		if(results==null)
			results = new NodeResults();
		return results;
	}
	
	@Override
	public String toString() {
		return levelTimeDivision.toString()+(StringUtils.isBlank(suffix) ? Constant.EMPTY_STRING : (Constant.CHARACTER_SPACE+suffix));
	}	
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, academicSession.getIdentifier(),levelTimeDivision.getUiString(),suffix,coordinator==null?Constant.EMPTY_STRING:coordinator.getCode());
	}
	private static final String LOG_FORMAT = ClassroomSession.class.getSimpleName()+"(AS=%s LEVEL=%s SUFFIX=%s COORDINATOR=%s)";
	
	public static final String FIELD_ACADEMIC_SESSION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_COORDINATOR = "coordinator";
	public static final String FIELD_NUMBER_OF_DIVISIONS = "numberOfDivisions";
	public static final String FIELD_SUFFIX = "suffix";
	
}
