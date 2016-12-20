package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.root.model.time.AbstractIdentifiablePeriod;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSessionDivision extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private ClassroomSession classroomSession;
	@ManyToOne @NotNull private TimeDivisionType timeDivisionType;
	
	@Column(nullable=false) @NotNull private Long numberOfSubjects=0l;
	@Embedded private NodeResults results = new NodeResults();
	
	private Boolean studentEvaluationRequired=Boolean.TRUE;
	private Boolean studentSubjectAttendanceAggregated=Boolean.TRUE;
	private Boolean studentRankable=Boolean.TRUE;
	private Boolean studentSubjectRankable=Boolean.TRUE;
	
	@Transient private IdentifiableRuntimeCollection<ClassroomSessionDivisionSubject> subjects;
	
	public ClassroomSessionDivision(ClassroomSession classroomSession,TimeDivisionType timeDivisionType, BigDecimal weight) {
		super();
		this.timeDivisionType = timeDivisionType;
		this.classroomSession = classroomSession;
		this.getGlobalIdentifierCreateIfNull().setWeight(weight);
	}
	
	public NodeResults getResults(){
		if(results==null)
			results = new NodeResults();
		return results;
	}
	
	public IdentifiableRuntimeCollection<ClassroomSessionDivisionSubject> getSubjects(){
		if(subjects == null)
			subjects = new IdentifiableRuntimeCollection<>();
		return subjects;
	}
	
	@Override
	public String toString() {
		return timeDivisionType.toString()+Constant.CHARACTER_SPACE+getGlobalIdentifier().getOrderNumber();
	}
	
	@Override
	public String getUiString() {
		return toString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSession.getIdentifier(),timeDivisionType.getCode(),globalIdentifier.getWeight(),globalIdentifier.getOrderNumber(),results.getLogMessage());
	}
	private static final String LOG_FORMAT = ClassroomSessionDivision.class.getSimpleName()+"(CLASS=%s TIMDIV=%s COEF=%s INDEX=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	public static final String FIELD_RESULTS = "results";
	public static final String FIELD_NUMBER_OF_SUBJECTS = "numberOfSubjects";
	
}
