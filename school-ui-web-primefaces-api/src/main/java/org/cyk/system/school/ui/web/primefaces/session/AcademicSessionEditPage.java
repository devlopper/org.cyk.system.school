package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Date;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionEditPage extends AbstractCrudOnePage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
	}
		
	public static class Form extends AbstractFormModel<AcademicSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputCalendar private Date fromDate;
		@Input @InputCalendar private Date toDate;
		@Input @InputCalendar private Date nextStartingDate;
		@Input @InputChoice @InputOneChoice @InputOneCombo private TimeDivisionType attendanceTimeDivisionType;
		@Input @InputBooleanButton private Boolean aggregateAttendance;
		@Input @InputChoice @InputOneChoice @InputOneCombo private TimeDivisionType classroomSessionTimeDivisionType;
		@Input @InputNumber private Long currentClassroomSessionDivisionIndex;
		
		@Override
		public void read() {
			super.read();
			fromDate = identifiable.getExistencePeriod().getFromDate();
			toDate = identifiable.getExistencePeriod().getToDate();
			
			attendanceTimeDivisionType = identifiable.getNodeInformations().getAttendanceTimeDivisionType();
			aggregateAttendance = identifiable.getNodeInformations().getAggregateAttendance();
			classroomSessionTimeDivisionType = identifiable.getNodeInformations().getClassroomSessionTimeDivisionType();
			currentClassroomSessionDivisionIndex = identifiable.getNodeInformations().getCurrentClassroomSessionDivisionIndex();
			
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.getExistencePeriod().setFromDate(fromDate);
			identifiable.getExistencePeriod().setToDate(toDate);
			
			identifiable.getNodeInformations().setAttendanceTimeDivisionType(attendanceTimeDivisionType);
			identifiable.getNodeInformations().setAggregateAttendance(aggregateAttendance);
			identifiable.getNodeInformations().setClassroomSessionTimeDivisionType(classroomSessionTimeDivisionType);
			identifiable.getNodeInformations().setCurrentClassroomSessionDivisionIndex(currentClassroomSessionDivisionIndex);
		}
		
		public static final String FIELD_FROM_DATE = "fromDate";
		public static final String FIELD_TO_DATE = "toDate";
		public static final String FIELD_NEXT_STARTING_DATE = "nextStartingDate";
		public static final String FIELD_ATTENDANCE_TIME_DIVISION_TYPE = "attendanceTimeDivisionType";
		public static final String FIELD_AGGREGATE_ATTENDANCE = "aggregateAttendance";
		public static final String FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE = "classroomSessionTimeDivisionType";
		public static final String FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX = "currentClassroomSessionDivisionIndex";
			
	}

}
