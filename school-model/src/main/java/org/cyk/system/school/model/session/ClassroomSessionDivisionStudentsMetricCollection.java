package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor @Deprecated
public class ClassroomSessionDivisionStudentsMetricCollection extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private ClassroomSessionDivision classroomSessionDivision;
	@ManyToOne @NotNull private MetricCollection metricCollection;
	
	public ClassroomSessionDivisionStudentsMetricCollection(ClassroomSessionDivision classroomSessionDivision,MetricCollection metricCollection) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
		this.metricCollection = metricCollection;
	}
	
	@Override
	public String toString() {
		return classroomSessionDivision.getCode()+Constant.CHARACTER_SPACE.toString()+metricCollection.getName();
	}
	
	/**/
	
	public static final String FIELD_CLASSROOMSESSIONDIVISION = "classroomSessionDivision";
	public static final String FIELD_METRICCOLLECTION = "metricCollection";
	
	/**/
	
}
