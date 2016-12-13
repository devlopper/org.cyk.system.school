package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.api.time.TimeDivisionTypeBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.file.FileRepresentationType;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.root.persistence.api.file.FileRepresentationTypeDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;

public class ClassroomSessionBusinessImpl extends AbstractTypedBusinessService<ClassroomSession, ClassroomSessionDao> implements ClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionBusinessImpl(ClassroomSessionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSession classroomSession, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSession.getLevelTimeDivision(),classroomSession.getSuffix()};
		return super.getPropertyValueTokens(classroomSession, name);
	}

	@Override
	public void computeResults(Collection<ClassroomSession> classroomSessions,Collection<StudentClassroomSession> studentClassroomSessions) {
		for(ClassroomSession classroomSession : classroomSessions){
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			Integer numberOfStudent = 0;
			for(StudentClassroomSession s : studentClassroomSessions){
				if(!s.getClassroomSession().equals(classroomSession) || s.getResults().getEvaluationSort().getAverage().getValue()==null)
					continue;
				
				s.setClassroomSession(classroomSession);
				
				weightedValues.add(new WeightedValue(s.getResults().getEvaluationSort().getAverage().getValue(),BigDecimal.ONE,Boolean.TRUE));
				if(s.getResults().getEvaluationSort().getAverage().getValue()==null)
					continue; 
				numberOfStudent++;
				if(classroomSession.getResults().getAverageHighest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSession.getResults().getAverageHighest())>0)
					classroomSession.getResults().setAverageHighest(s.getResults().getEvaluationSort().getAverage().getValue());
				if(classroomSession.getResults().getAverageLowest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSession.getResults().getAverageLowest())<0)
					classroomSession.getResults().setAverageLowest(s.getResults().getEvaluationSort().getAverage().getValue());	
			}
			if(weightedValues.isEmpty()){
				
			}else{
				Average average = inject(MathematicsBusiness.class).average(weightedValues, null, null);
				classroomSession.getResults().setAverage(average.getValue());
				classroomSession.getResults().setNumberOfStudent(numberOfStudent);
			}
			dao.update(classroomSession);
		}
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession) {
		return dao.readByAcademicSession(academicSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByAcademicSessionByTeacher(AcademicSession academicSession, Teacher teacher) {
		return dao.readByAcademicSessionByTeacher(academicSession,teacher);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByAcademicSessionByCoordinator(AcademicSession academicSession, Teacher coordinator) {
		return dao.readByAcademicSessionByCoordinator(academicSession,coordinator);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return dao.readByLevelTimeDivision(levelTimeDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession) {
		CommonNodeInformations commonNodeInformations = classroomSession.getLevelTimeDivision().getLevel().getLevelName().getNodeInformations();
		if(commonNodeInformations==null)
			commonNodeInformations = classroomSession.getAcademicSession().getNodeInformations();
		if(commonNodeInformations==null)
			commonNodeInformations = classroomSession.getAcademicSession().getSchool().getNodeInformations();
		return commonNodeInformations;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<FileRepresentationType> findStudentClassroomSessionDivisionResultsFileRepresentationTypes(Collection<ClassroomSession> classroomSessions) {
		Set<String> codes = new LinkedHashSet<>();
		for(ClassroomSession classroomSession : classroomSessions)
			codes.add(inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession).getStudentClassroomSessionDivisionResultsReportTemplate().getCode());
		return inject(FileRepresentationTypeDao.class).readByGlobalIdentifierCodes(codes);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSession> findByStudentClassroomSessionDivisions(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		Collection<ClassroomSession> classroomSessions = new ArrayList<>();
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions)
			classroomSessions.add(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession());
		return classroomSessions;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond) {
		return millisecond==null?BigDecimal.ZERO
				:inject(TimeDivisionTypeBusiness.class).convertToDivisionDuration(findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType(), millisecond);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration) {
		return duration==null?0l:inject(TimeDivisionTypeBusiness.class).convertToMillisecond(findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType(), duration);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, String suffix) {
		return dao.readByAcademicSessionByLevelTimeDivisionBySuffix(academicSession,levelTimeDivision,suffix);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByAcademicSessionByLevelGroup(AcademicSession academicSession, LevelGroup levelGroup) {
		return dao.readByAcademicSessionByLevelGroup(academicSession,levelGroup);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSession> findByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession,LevelGroup levelGroup, Teacher teacher) {
		return dao.readByAcademicSessionByLevelGroupByTeacher(academicSession,levelGroup,teacher);
	}

	
	@Override
	public Collection<ClassroomSession> findByLevelNameBySuffix(String levelNameCode, String suffix) {
		if(StringUtils.isNotBlank(suffix))
			return dao.readByLevelNameBySuffix(levelNameCode,suffix);
		return dao.readWhereSuffixIsNullByLevelName(levelNameCode);
	}

	
}
