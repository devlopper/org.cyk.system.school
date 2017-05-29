package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.company.business.api.sale.SaleBusiness;
import org.cyk.system.company.model.sale.Sale;
import org.cyk.system.company.model.sale.SaleIdentifiableGlobalIdentifier;
import org.cyk.system.company.persistence.api.sale.SaleIdentifiableGlobalIdentifierDao;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

@Stateless
public class StudentClassroomSessionBusinessImpl extends AbstractStudentResultsBusinessImpl<StudentClassroomSession,StudentClassroomSessionDao,ClassroomSession, StudentClassroomSessionDivision> implements StudentClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	
	@Inject private StudentClassroomSessionDivisionSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao;
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	@Inject private ClassroomSessionDao classroomSessionDao;
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public StudentClassroomSessionBusinessImpl(StudentClassroomSessionDao dao) {
		super(dao); 
	}
	
	@Override
	public StudentClassroomSession instanciateOne(Student student, ClassroomSession classroomSession) {
		StudentClassroomSession studentClassroomSession = instanciateOne();
		studentClassroomSession.setStudent(student);
		studentClassroomSession.setClassroomSession(classroomSession);
		return studentClassroomSession;
	}
	
	@Override
	protected void afterCreate(StudentClassroomSession studentClassroomSession) {
		super.afterCreate(studentClassroomSession);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
		if(Boolean.TRUE.equals(studentClassroomSession.getCascadeOperationToChildren())){
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisionDao.readByClassroomSession(studentClassroomSession.getClassroomSession()))
				studentClassroomSessionDivisions.add(new StudentClassroomSessionDivision(studentClassroomSession.getStudent(), classroomSessionDivision));
		}
		cascade(studentClassroomSession, studentClassroomSessionDivisions, Crud.CREATE);
	}
	
	private void cascade(StudentClassroomSession studentClassroomSession,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions,Crud crud){
		new CascadeOperationListener.Adapter.Default<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDao,StudentClassroomSessionDivisionBusiness>(null
				,inject(StudentClassroomSessionDivisionBusiness.class))
			.operate(studentClassroomSessionDivisions, crud);
		commonUtils.increment(Integer.class, studentClassroomSession.getClassroomSession().getResults(), NodeResults.FIELD_NUMBER_OF_STUDENT
				, Crud.CREATE.equals(crud)?1:Crud.DELETE.equals(crud)?-1:0);
		classroomSessionDao.update(studentClassroomSession.getClassroomSession());
		if(Crud.CREATE.equals(crud)){
			/*
			Customer customer = inject(CustomerDao.class).read(studentClassroomSession.getStudent().getCode());
			if(customer==null){
				customer = inject(CustomerBusiness.class).instanciateOne();
				customer.setCode(studentClassroomSession.getStudent().getCode());
				customer.setPerson(studentClassroomSession.getStudent().getPerson());
				inject(CustomerBusiness.class).create(customer);
			}
			
			Sale sale = inject(SaleBusiness.class).instanciateOne();
			sale.setName("School Fees");
			sale.setCustomer(customer);
			inject(SaleBusiness.class).create(sale);
			
			SaleIdentifiableGlobalIdentifier saleIdentifiableGlobalIdentifier = new SaleIdentifiableGlobalIdentifier(sale, studentClassroomSession);
			inject(SaleIdentifiableGlobalIdentifierBusiness.class).create(saleIdentifiableGlobalIdentifier);
			*/
		}else if(Crud.DELETE.equals(crud)){
			for(SaleIdentifiableGlobalIdentifier saleIdentifiableGlobalIdentifier : inject(SaleIdentifiableGlobalIdentifierDao.class).readByIdentifiableGlobalIdentifier(studentClassroomSession)){
				Sale sale = saleIdentifiableGlobalIdentifier.getSale();
				inject(SaleBusiness.class).delete(sale);
				
			}
		}
	}
	
	@Override
	protected void beforeUpdate(StudentClassroomSession studentClassroomSession) {
		super.beforeUpdate(studentClassroomSession);
		StudentClassroomSession currentStudentClassroomSession = dao.read(studentClassroomSession.getIdentifier());
		if(currentStudentClassroomSession.getClassroomSession().equals(studentClassroomSession.getClassroomSession())){
			
		}else{
			//logTrace("Moving student from classroom session {} to {}", currentStudentClassroomSession.getClassroomSession(),studentClassroomSession.getClassroomSession());
			
		}
		if(studentClassroomSession.getDetailCollection()!=null && studentClassroomSession.getDetailCollection().isSynchonizationEnabled()){
			inject(StudentClassroomSessionDivisionBusiness.class).update(studentClassroomSession.getDetailCollection().getCollection());
		}
	}
	
	@Override
	protected void beforeDelete(StudentClassroomSession studentClassroomSession) {
		super.beforeDelete(studentClassroomSession);
		cascade(studentClassroomSession, studentClassroomSessionDivisionDao.readByStudentByClassroomSession(studentClassroomSession.getStudent()
				, studentClassroomSession.getClassroomSession()), Crud.DELETE);
	}
		
	/**/
	
	@Override
	public Collection<StudentClassroomSession> updateAverage(Collection<ClassroomSession> classroomSessions,BusinessServiceCallArguments<StudentClassroomSession> callArguments) {
		/*
		 * Data loading
		 */
		Collection<StudentClassroomSession> studentClassroomSessions = dao.readByClassroomSessions(classroomSessions);
		//Collection<ClassroomSessionDivision> classroomSessionDivisions = classroomSessionDivisionDao.readByClassroomSessions(classroomSessions);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = studentClassroomSessionDivisionDao.readByClassroomSessions(classroomSessions);
		//logTrace("Loaded data. StudentSubjectEvaluation={} , StudentSubject={} , StudentClassroomSessionDivision={}"
		//		,studentSubjectEvaluations.size(),studentSubjects.size(),studentClassroomSessionDivisions.size());
		
		setCallArgumentsObjects(callArguments, studentClassroomSessions);
		/*
		 * Data computing
		 */
		
		updateAverage(classroomSessions, studentClassroomSessions,studentClassroomSessionDivisions, callArguments);
		
		Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>();
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions)
			classroomSessionDivisions.add(studentClassroomSessionDivision.getClassroomSessionDivision());
		
		inject(ClassroomSessionDivisionBusiness.class).computeResults(classroomSessionDivisions, studentClassroomSessionDivisions);
		inject(ClassroomSessionBusiness.class).computeResults(classroomSessions, studentClassroomSessions);
		
		return studentClassroomSessions;
	}
	
	/**/
	
	@Override
	protected Class<StudentClassroomSession> getResultClass() {
		return StudentClassroomSession.class;
	}
	
	@Override
	protected Class<StudentClassroomSessionDivision> getDetailsClass() {
		return StudentClassroomSessionDivision.class;
	}
	
	@Override
	protected WeightedValue weightedValue(StudentClassroomSessionDivision detail) {
		return new WeightedValue(detail.getResults().getEvaluationSort().getAverage().getValue(),detail.getClassroomSessionDivision().getWeight(),Boolean.FALSE);
	}

	@Override
	protected Student student(StudentClassroomSessionDivision detail) {
		return detail.getStudent();
	}
 
	@Override
	protected Collection<StudentClassroomSession> readResults(Collection<ClassroomSession> levels) {
		return dao.readByClassroomSessions(levels);
	}

	@Override
	protected Collection<StudentClassroomSessionDivision> readDetails(Collection<ClassroomSession> levels,Boolean keepDetails) {
		//structure
		Collection<ClassroomSessionDivision> classroomSessionDivisions = classroomSessionDivisionDao.readByClassroomSessions(levels);
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessions(levels);
		//student data
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = studentClassroomSessionDivisionDao.readByClassroomSessions(levels);
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectDao.readByClassroomSessions(levels);
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessions(levels);
		
		studentSubjectBusiness.updateAverage(subjects, studentSubjects, evaluatedStudents,null);
		
		studentClassroomSessionDivisionBusiness.updateAverage(classroomSessionDivisions, studentClassroomSessionDivisions, studentSubjects,null);
		
		return studentClassroomSessionDivisions;
	}
	
	@Override
	protected ClassroomSession level(StudentClassroomSession result) {
		return result.getClassroomSession();
	}
	
	@Override
	protected ClassroomSession level(StudentClassroomSessionDivision detail) {
		return detail.getClassroomSessionDivision().getClassroomSession();
	}
	
	@Override
	protected IntervalCollection averageAppreciatedIntervalCollection(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionAverageScale();
	}
	
	@Override
	protected IntervalCollection averagePromotedIntervalCollection(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionAveragePromotionScale();
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return dao.readByClassroomSessions(classroomSessions);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByAcademicSession(AcademicSession academicSession) {
		return dao.readByAcademicSession(academicSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentClassroomSession findByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student,classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return dao.readByLevelTimeDivision(levelTimeDivision);
	}

	/**/
	
	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSession> levels) {
		return lectureDao.readByClassroomSessions(levels);
	}

	@Override
	protected ClassroomSession level(Lecture lecture) {
		return lecture.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession();
	}
	
	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentClassroomSession studentClassroomSession) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	protected Long getAttendableDuration(StudentClassroomSession studentClassroomSession) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByCriteria(SearchCriteria criteria) {
		prepareFindByCriteria(criteria);
		return dao.readByCriteria(criteria);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Long countByCriteria(SearchCriteria criteria) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public StudentClassroomSession instanciateOne(String[] values) {
		StudentClassroomSession studentClassroomSession = instanciateOne();
		Integer index = 0;
		studentClassroomSession.setStudent(inject(StudentDao.class).read(values[index++]));
		studentClassroomSession.setClassroomSession(inject(ClassroomSessionDao.class).read(values[index++]));
		return studentClassroomSession;
	}
	
	/**/
	
	

}
