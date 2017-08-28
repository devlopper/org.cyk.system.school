package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.api.file.report.ReportTemplateDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.session.SchoolBusiness;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.SchoolDao;
import org.cyk.utility.common.helper.FieldHelper;

public class SchoolBusinessImpl extends AbstractTypedBusinessService<School, SchoolDao> implements SchoolBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
		
	@Inject
	public SchoolBusinessImpl(SchoolDao dao) {
		super(dao);  
	}
	
	@Override
	public School instanciateOne() {
		School school = super.instanciateOne();
		school.setOwnedCompany(inject(OwnedCompanyBusiness.class).findDefaulted());	
		return school;
	}

	@Override
	public School instanciateOne(String[] values) {
		School school = instanciateOne();
		Integer index = 0;
		school.setCode(values[index++]);
		school.setName(values[index++]);
		school.getNodeInformations().setClassroomSessionTimeDivisionType(inject(TimeDivisionTypeDao.class).read(values[index++]));
		school.getNodeInformations().setCurrentClassroomSessionDivisionIndex(new Long(values[index++]));
		school.getNodeInformations().setClassroomSessionDivisionOrderNumberInterval(inject(IntervalDao.class).read(values[index++]));
		
		school.getNodeInformations().setStudentClassroomSessionDivisionResultsReportTemplate(inject(ReportTemplateDao.class).read(values[index++]));
		school.getNodeInformations().setStudentSubjectAverageScale(inject(IntervalCollectionDao.class).read(values[index++]));
		school.getNodeInformations().setStudentClassroomSessionDivisionAverageScale(inject(IntervalCollectionDao.class).read(values[index++]));
		school.getNodeInformations().setStudentClassroomSessionAverageScale(inject(IntervalCollectionDao.class).read(values[index++]));
		school.getNodeInformations().setStudentClassroomSessionAveragePromotionScale(inject(IntervalCollectionDao.class).read(values[index++]));
		school.getNodeInformations().setAttendanceTimeDivisionType(inject(TimeDivisionTypeDao.class).read(values[index++]));
		school.getNodeInformations().setEvaluationPassAverage(commonUtils.getBigDecimal(values[index++]));
		
		if(values.length > index && StringUtils.isNotBlank(values[index])){
			school.setDefaulted(Boolean.valueOf(values[index]));
		}
		
		return school;
	}
	
	public static class BuilderOneDimensionArray extends org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<School> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(School.class);
			addFieldCodeName();
			addParameterArrayElementString(FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CLASSROOM_SESSION_DIVISION_ORDER_NUMBER_INTERVAL)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_TEMPLATE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_SUBJECT_AVERAGESCALE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_AVERAGE_SCALE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_SCALE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_PROMOTION_SCALE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_ATTENDANCE_TIME_DIVISION_TYPE)
					,FieldHelper.getInstance().buildPath(School.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_EVALUATION_PASS_AVERAGE)
					,FieldHelper.getInstance().buildPath(School.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_DEFAULTED)
					);
		}
		
	}
}
