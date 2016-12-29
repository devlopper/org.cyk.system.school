package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.language.LanguageCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.root.model.party.person.MedicalInformations;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.Setter;

public class StudentBusinessImpl extends AbstractActorBusinessImpl<Student, StudentDao,SearchCriteria> implements StudentBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentBusinessImpl(StudentDao dao) {
		super(dao);  
	}
	
	@Override
	protected Collection<? extends org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl.Listener<?>> getListeners() {
		return Listener.COLLECTION;
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<Student> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}	
	
	/**/

	public static interface Listener extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener<Student>{
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/

		public static class Adapter extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener.Adapter.Default<Student> implements Listener, Serializable {
			private static final long serialVersionUID = -1625238619828187690L;
			
			/**/
			@Getter @Setter
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = -1625238619828187690L;
				
				@Override
				public void afterCreate(Student student) {
					super.afterCreate(student);
					if(containsCascadeToClass(StudentClassroomSession.class) && student.getStudentClassroomSession()!=null && student.getStudentClassroomSession().getClassroomSession()!=null 
							&& student.getStudentClassroomSession().getIdentifier()==null){
						student.getStudentClassroomSession().setStudent(student);
						inject(StudentClassroomSessionBusiness.class).create(student.getStudentClassroomSession());
					}		
				}
				
				/**/
				
				public static class EnterpriseResourcePlanning extends StudentBusinessImpl.Listener.Adapter.Default implements Serializable {
					
					private static final long serialVersionUID = 1L;

					public EnterpriseResourcePlanning() {
						addCascadeToClass(StudentClassroomSession.class).addCascadeToReportTemplateCodes(SchoolConstant.Code.ReportTemplate.STUDENT_REGISTRATION_CERTIFICATE,
								SchoolConstant.Code.ReportTemplate.STUDENT_TUITION_CERTIFICATE);
					}
					
					@Override
					public void afterInstanciateOne(UserAccount userAccount, Student student) {
						super.afterInstanciateOne(userAccount, student);
						student.setStudentClassroomSession(new StudentClassroomSession(student, null));
						student.getPerson().getExtendedInformations().setLanguageCollection(inject(LanguageCollectionBusiness.class).instanciateOne(userAccount));
						student.getPerson().setMedicalInformations(new MedicalInformations(student.getPerson()));
					}
					
					@Override
					public void beforeCreate(Student student) {
						super.beforeCreate(student);
						if(StringUtils.isBlank(student.getCode())){
							NumberBusiness.FormatArguments orderNumberFormatArguments = new FormatArguments();
							orderNumberFormatArguments.setWidth(4);
							student.setCode(getCodePrefix()+Constant.CHARACTER_SLASH+inject(TimeBusiness.class).findYear(inject(AcademicSessionBusiness.class).findCurrent(null).getBirthDate())
									+inject(PersonBusiness.class).findInitials(student.getPerson())+inject(NumberBusiness.class).format(inject(StudentDao.class).countAll()+1,orderNumberFormatArguments)
									+Constant.CHARACTER_HYPHEN+student.getAdmissionLevelTimeDivision().getLevel().getGroup().getCode()
									);
						}
					}
					
					@Override
					public void afterCreate(Student student) {
						super.afterCreate(student);
						/*Customer customer = new Customer();
						customer.setPerson(student.getPerson());
						customer.setCode(student.getCode());
						customer.setName(student.getName());
						inject(CustomerBusiness.class).create(customer);
						*/
					}
					
				}
			}
		}
	}
	
}
