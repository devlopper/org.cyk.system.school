package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionStudentsMetricCollectionDetails extends AbstractOutputDetails<ClassroomSessionDivisionStudentsMetricCollection> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String classroomSessionDivision,metricCollection;
	
	public ClassroomSessionDivisionStudentsMetricCollectionDetails(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection) {
		super(classroomSessionDivisionStudentsMetricCollection);
		classroomSessionDivision = formatUsingBusiness(classroomSessionDivisionStudentsMetricCollection.getClassroomSessionDivision());
		metricCollection = formatUsingBusiness(classroomSessionDivisionStudentsMetricCollection.getMetricCollection());
	}
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String FIELD_METRIC_COLLECTION = "metricCollection";
	
}