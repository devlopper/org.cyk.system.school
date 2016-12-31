package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.persistence.api.file.report.ReportTemplateDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.session.SchoolBusiness;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.SchoolDao;

public class SchoolBusinessImpl extends AbstractTypedBusinessService<School, SchoolDao> implements SchoolBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	
	@Inject
	public SchoolBusinessImpl(SchoolDao dao) {
		super(dao);  
	}

	@Override
	public School findDefault() {
		return dao.readByOwnedCompany(ownedCompanyBusiness.findDefaultOwnedCompany());
	}
	
	@Override
	public School instanciateOne() {
		School school = super.instanciateOne();
		school.setOwnedCompany(inject(OwnedCompanyBusiness.class).findDefaultOwnedCompany());		
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
		
		return school;
	}
}
