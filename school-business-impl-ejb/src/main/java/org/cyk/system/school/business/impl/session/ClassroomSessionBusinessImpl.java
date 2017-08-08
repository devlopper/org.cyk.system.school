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
import org.cyk.system.root.business.api.value.MeasureBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.file.FileRepresentationType;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.root.model.search.AbstractFieldValueSearchCriteriaSet;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.file.FileRepresentationTypeDao;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSuffixDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.utility.common.Constant;

public class ClassroomSessionBusinessImpl extends AbstractTypedBusinessService<ClassroomSession, ClassroomSessionDao> implements ClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionBusinessImpl(ClassroomSessionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSession classroomSession, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSession.getAcademicSession(),classroomSession.getLevelTimeDivision(),classroomSession.getSuffix()};
		return super.getPropertyValueTokens(classroomSession, name);
	}
	
	@Override
	protected void afterCreate(ClassroomSession classroomSession) {
		super.afterCreate(classroomSession);
		if(classroomSession.getDivisions().isSynchonizationEnabled())
			inject(ClassroomSessionDivisionBusiness.class).create(classroomSession.getDivisions().getCollection());
	}

	@Override
	protected void afterUpdate(ClassroomSession classroomSession) {
		super.afterUpdate(classroomSession);
		if(classroomSession.getSubjects().isSynchonizationEnabled())
			inject(ClassroomSessionSubjectBusiness.class).update(classroomSession.getSubjects().getCollection());
	}
	
	@Override
	protected void beforeDelete(ClassroomSession classroomSession) {
		super.beforeDelete(classroomSession);
		inject(ClassroomSessionDivisionBusiness.class).delete(inject(ClassroomSessionDivisionDao.class).readByClassroomSession(classroomSession));
		inject(ClassroomSessionSubjectBusiness.class).delete(inject(ClassroomSessionSubjectDao.class).readByClassroomSession(classroomSession));
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
	
	@Override
	public ClassroomSession findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(String levelTimeDivisionCode,String suffixCode) {
		LevelTimeDivision levelTimeDivision = inject(LevelTimeDivisionDao.class).read(levelTimeDivisionCode);
		if(StringUtils.isBlank(suffixCode))
			return dao.readWhereSuffixIsNullByAcademicSessionByLevelTimeDivision(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted(), levelTimeDivision);
		return findByAcademicSessionByLevelTimeDivisionBySuffix(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted(), levelTimeDivision
				, inject(ClassroomSessionSuffixDao.class).read(suffixCode));
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
		TimeDivisionType timeDivisionType = findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType();
		return millisecond==null?BigDecimal.ZERO:inject(MeasureBusiness.class).computeQuotient(timeDivisionType.getMeasure(), new BigDecimal(millisecond));
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration) {
		TimeDivisionType timeDivisionType = findCommonNodeInformations(classroomSession).getAttendanceTimeDivisionType();
		return duration==null?0l:inject(MeasureBusiness.class).computeMultiple(timeDivisionType.getMeasure(), duration).longValue();
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, String suffixCode) {
		return findByAcademicSessionByLevelTimeDivisionBySuffix(academicSession,levelTimeDivision,inject(ClassroomSessionSuffixDao.class).read(suffixCode));
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, ClassroomSessionSuffix suffix) {
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

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSession> findByLevelNameBySuffix(String levelNameCode, String suffix) {
		if(StringUtils.isNotBlank(suffix))
			return dao.readByLevelNameBySuffix(levelNameCode,suffix);
		return dao.readWhereSuffixIsNullByLevelName(levelNameCode);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSession> findByLevelName(String levelNameCode) {
		return findByLevelNameBySuffix(levelNameCode,null);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSession> findByCriteria(SearchCriteria searchCriteria) {
		prepareFindByCriteria(searchCriteria);
		return dao.readByCriteria(searchCriteria);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Long countByCriteria(SearchCriteria searchCriteria) {
		prepareFindByCriteria(searchCriteria);
		return dao.countByCriteria(searchCriteria);
	}
	
	@Override
	protected void prepareFindByCriteria(AbstractFieldValueSearchCriteriaSet searchCriteria) {
		super.prepareFindByCriteria(searchCriteria);
		if(((SearchCriteria)searchCriteria).getAcademicSessions().isEmpty())
			((SearchCriteria)searchCriteria).addAcademicSession(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted());
	}
	
	@Override
	public ClassroomSession instanciateOne() {
		ClassroomSession classroomSession = super.instanciateOne();
		classroomSession.setAcademicSession(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted());
		return classroomSession;
	}
	
	@Override
	protected ClassroomSession __instanciateOne__(String[] values,org.cyk.system.root.business.api.TypedBusiness.InstanciateOneListener<ClassroomSession> listener) {
		ClassroomSession classroomSession = listener.getInstance();
		set(listener.getSetListener(), ClassroomSession.FIELD_ACADEMIC_SESSION);
		set(listener.getSetListener(), ClassroomSession.FIELD_LEVEL_TIME_DIVISION);
		set(listener.getSetListener(), ClassroomSession.FIELD_SUFFIX);
		set(listener.getSetListener(), ClassroomSession.FIELD_COORDINATOR);
		
		Integer index = listener.getSetListener().getIndex();
		String value;
		String classroomSessionTimeDivisionTypeCode = values[index++];
		classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
		if(StringUtils.isNotBlank(value = values[index++])){
			for(String classroomSessionDivisionInfos : StringUtils.split(value,Constant.CHARACTER_VERTICAL_BAR.toString())){
				String[] array = StringUtils.split(classroomSessionDivisionInfos, Constant.CHARACTER_COMA.toString());
				ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class)
					.instanciateOne(new String[]{null,classroomSessionTimeDivisionTypeCode,commonUtils.getValueAt(array, 0),commonUtils.getValueAt(array, 1)
						,commonUtils.getValueAt(array, 2),commonUtils.getValueAt(array, 3),commonUtils.getValueAt(array, 4),commonUtils.getValueAt(array, 5)
						,commonUtils.getValueAt(array, 6),commonUtils.getValueAt(values, index),commonUtils.getValueAt(values, index+1)
						,commonUtils.getValueAt(values, index+2)});
				classroomSessionDivision.setClassroomSession(classroomSession);
				classroomSession.getDivisions().getCollection().add(classroomSessionDivision);
			}
		}
		return classroomSession;
	}
	
	@Override
	public ClassroomSession instanciateOne(String levelTimeDivisionCode,String suffixCode,String coordinatorCode,String timeDivisionTypeCode,String[][] divisions,String[][] subjects,String[][] evaluationTypes,String[][] metricCollections){
		return inject(ClassroomSessionBusiness.class)
    		.instanciateOne(new String[]{null,levelTimeDivisionCode,suffixCode,coordinatorCode,timeDivisionTypeCode
    		,commonUtils.convertToString(divisions, Constant.CHARACTER_VERTICAL_BAR, Constant.CHARACTER_COMA)
    		,commonUtils.convertToString(subjects, Constant.CHARACTER_VERTICAL_BAR, Constant.CHARACTER_COMA)
    		,commonUtils.convertToString(evaluationTypes, Constant.CHARACTER_VERTICAL_BAR, Constant.CHARACTER_COMA)
    		,commonUtils.convertToString(metricCollections, Constant.CHARACTER_VERTICAL_BAR, Constant.CHARACTER_COMA)
        		
    		});
	}
	
}
