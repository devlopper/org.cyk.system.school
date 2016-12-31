package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.business.api.session.LevelNameBusiness;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.persistence.api.session.LevelNameDao;

public class LevelNameBusinessImpl extends AbstractEnumerationBusinessImpl<LevelName, LevelNameDao> implements LevelNameBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelNameBusinessImpl(LevelNameDao dao) {
		super(dao); 
	}   
	
	@Override
	public LevelName instanciateOne(String[] values) {
		LevelName levelName = super.instanciateOne(values);
		Integer index = 10;
		String value;
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setClassroomSessionTimeDivisionType(read(TimeDivisionType.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setCurrentClassroomSessionDivisionIndex(Long.parseLong(value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setClassroomSessionDivisionOrderNumberInterval(read(Interval.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentClassroomSessionDivisionResultsReportTemplate(read(ReportTemplate.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentClassroomSessionDivisionResultsReportSigner(read(Person.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentSubjectAverageScale(read(IntervalCollection.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentClassroomSessionDivisionAverageScale(read(IntervalCollection.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentClassroomSessionAverageScale(read(IntervalCollection.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setStudentClassroomSessionAveragePromotionScale(read(IntervalCollection.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setAttendanceTimeDivisionType(read(TimeDivisionType.class, value));
		if(StringUtils.isNotBlank(value = values[index++]))
			levelName.getNodeInformations().setEvaluationPassAverage(commonUtils.getBigDecimal(value));
		return levelName;
	}
}
