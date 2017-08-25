package org.cyk.system.school.business.impl.iesa;

import java.io.FileInputStream;
import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.DataSet;
import org.cyk.system.root.business.impl.globalidentification.GlobalIdentifierBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.geography.ContactCollection;
import org.cyk.system.root.model.geography.ElectronicMail;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobInformations;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonExtendedInformations;
import org.cyk.system.root.model.party.person.PersonRelationship;
import org.cyk.system.root.model.party.person.PersonRelationshipExtremity;
import org.cyk.system.root.model.party.person.PersonRelationshipTypeRole;
import org.cyk.system.root.model.security.Credentials;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.root.persistence.api.security.RoleDao;
import org.cyk.system.root.persistence.api.security.SoftwareDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;
import org.cyk.utility.common.helper.FieldHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.InstanceHelper.Pool;
import org.cyk.utility.common.helper.StringHelper;

public class Iesa_2016_2017_DataSet extends DataSet implements Serializable {

	private static final long serialVersionUID = 1L;

	public Iesa_2016_2017_DataSet() {
		super(SchoolBusinessLayer.class);
		
		setExcelWorkbookFileName("data\\iesa\\IESA_2017_2016.xlsx");
    	
    	addClass(GlobalIdentifier.class,new GlobalIdentifierBusinessImpl.BuilderOneDimensionArray(){
    		private static final long serialVersionUID = 1L;

			@Override
    		protected GlobalIdentifier __execute__() {
    			Object[] array = getInput();
    			if(!StringHelper.getInstance().isBlank((String)array[10])){
    				String date = (String)array[10];
    				String[] p = StringUtils.split(StringUtils.substringBefore(date, Constant.CHARACTER_SPACE.toString()),"-");
    				array[10] = p[2]+"/"+p[1]+"/"+p[0];
    			}
    			return super.__execute__();
    		}
    	});
    	/*
    	addClass(JobTitle.class,new AbstractIdentifiableBusinessServiceImpl.OneDimensionArrayBuilderFromGlobalIdentifier<JobTitle>(JobTitle.class));
    	instanceKeyBuilderMap.put(JobTitle.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.TRUE));
    	
    	InstanceHelper.Pool.getInstance().load(Person.class);
    	addClass(Person.class,new AbstractIdentifiableBusinessServiceImpl.OneDimensionArrayBuilderFromGlobalIdentifier<Person>(Person.class){
			private static final long serialVersionUID = 1L;
    		
    		protected void onIdentifierIsNull(Person person) {
    			super.onIdentifierIsNull(person);
    			person.setContactCollection(new ContactCollection());
				if(person.getExtendedInformations().getSignatureSpecimen()!=null && !StringHelper.getInstance().isBlank(person.getExtendedInformations().getSignatureSpecimen().getUri())){
					String name = person.getExtendedInformations().getSignatureSpecimen().getUri()+"."+person.getExtendedInformations().getSignatureSpecimen().getExtension();
					String path = GlobalIdentifierBusinessImpl.BuilderOneDimensionArray.IMAGE_DIRECTORY_PATH+name;
					byte[] bytes;
					try {
						bytes = IOUtils.toByteArray(new FileInputStream(path));
						person.getExtendedInformations().setSignatureSpecimen(inject(FileBusiness.class).process(bytes, name));
					} catch (Exception e) {
						e.printStackTrace();
					}
				}else if(person.getExtendedInformations().getSignatureSpecimen()!=null){
					person.getExtendedInformations().setSignatureSpecimen(null);
				}
    		}
    		
    	}.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			,Person.FIELD_LASTNAMES,Person.FIELD_SURNAME,FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER
    					,GlobalIdentifier.FIELD_BIRTH_LOCATION,AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_OTHER_DETAILS),Person.FIELD_NATIONALITY
    					,Person.FIELD_SEX
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_TITLE)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_FUNCTION)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_TITLE)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_SIGNATURE_SPECIMEN,File.FIELD_URI)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_SIGNATURE_SPECIMEN,File.FIELD_EXTENSION))
    			);
    	
    	instanceKeyBuilderMap.put(Person.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.TRUE));
    	
    	addClass(UserAccount.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<UserAccount>(UserAccount.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected UserAccount __execute__() {
    			Person person = Pool.getInstance().get(Person.class, getInput()[2]);
				if(person==null)
					return null;
				UserAccount userAccount = super.__execute__();
				userAccount.setUser(Pool.getInstance().get(Person.class, getInput()[2]));
				userAccount.getCredentials().setSoftware(inject(SoftwareDao.class).readDefaulted());
				userAccount.getRoles().add(inject(RoleDao.class).read(SchoolConstant.Code.Role.TEACHER));
				return userAccount;
    		}
    	}.addParameterArrayElementStringIndexInstance(0,FieldHelper.getInstance().buildPath(UserAccount.FIELD_CREDENTIALS,Credentials.FIELD_USERNAME)
    			,1,FieldHelper.getInstance().buildPath(UserAccount.FIELD_CREDENTIALS,Credentials.FIELD_PASSWORD)));
    	
    	instanceKeyBuilderMap.put(UserAccount.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE));
    	
    	addClass(Student.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<Student>(Student.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Student __execute__() {
    			Student student = super.__execute__();
				if(student.getPerson()==null)
					return null;
				student.setImage(student.getPerson().getImage());
				return student;
    		}
    	}.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_CODE),Student.FIELD_PERSON));
    	
    	instanceKeyBuilderMap.put(Student.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.TRUE));
    	*/
    	addClass(PersonRelationship.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<PersonRelationship>(PersonRelationship.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected PersonRelationship __execute__() {
    			PersonRelationship personRelationship = super.__execute__();
				if(personRelationship.getExtremity1().getPerson()==null || personRelationship.getExtremity2().getPerson()==null)
					return null;
				Boolean person1IsStudent = Pool.getInstance().get(Student.class, getInput()[0],Boolean.TRUE)!=null;
				
				PersonRelationshipExtremity parentExtremity,studentExtremity;
				String studentExtremityRole,parentExtremityRole = "FAMILYFATHER".equals(getInput()[1]) ? RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_FATHER
						: RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_MOTHER;
				if(Boolean.TRUE.equals(person1IsStudent)){
					studentExtremity = personRelationship.getExtremity1();
					parentExtremity = personRelationship.getExtremity2();
				}else{
					studentExtremity = personRelationship.getExtremity2();
					parentExtremity = personRelationship.getExtremity1();
				}
				
				if(studentExtremity.getPerson().getSex() == null || RootConstant.Code.Sex.MALE.equals(studentExtremity.getPerson().getSex().getCode()))
					studentExtremityRole = RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_SON;
				else
					studentExtremityRole = RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_DAUGHTER;
				
				studentExtremity.setRole(Pool.getInstance().get(PersonRelationshipTypeRole.class, studentExtremityRole,Boolean.TRUE));
				parentExtremity.setRole(Pool.getInstance().get(PersonRelationshipTypeRole.class, parentExtremityRole,Boolean.TRUE));
				
				return personRelationship;
    		}
    	}.addParameterArrayElementStringIndexInstance(0,FieldHelper.getInstance().buildPath(PersonRelationship.FIELD_EXTREMITY_1,PersonRelationshipExtremity.FIELD_PERSON)
    	    	,2,FieldHelper.getInstance().buildPath(PersonRelationship.FIELD_EXTREMITY_2,PersonRelationshipExtremity.FIELD_PERSON)));
    	
    	instanceKeyBuilderMap.put(PersonRelationship.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE));
    	
    	/*
    	addClass(Teacher.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<Teacher>(Teacher.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Teacher __execute__() {
    			Teacher teacher = super.__execute__();
				if(teacher.getPerson()==null)
					return null;
				return teacher;
    		}
    	}.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_CODE),Student.FIELD_PERSON));
    	
    	instanceKeyBuilderMap.put(Teacher.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.TRUE));
    	
    	addClass(ElectronicMail.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<ElectronicMail>(ElectronicMail.class){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected ElectronicMail __execute__() {
    			Person person = Pool.getInstance().get(Person.class, getInput()[1]);
				if(person==null)
					return null;
				ElectronicMail electronicMail = super.__execute__();
				electronicMail.setCollection(person.getContactCollection());
				
				return electronicMail;
    		}
    	}.addParameterArrayElementStringIndexInstance(0,ElectronicMail.FIELD_ADDRESS));
    	
    	instanceKeyBuilderMap.put(Teacher.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE));
    	
    	addClass(ClassroomSession.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<ClassroomSession>(ClassroomSession.class){
			private static final long serialVersionUID = 1L;
    		
    	}.addParameterArrayElementStringIndexInstance(2,ClassroomSession.FIELD_COORDINATOR));
    	
    	instanceKeyBuilderMap.put(ClassroomSession.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE){
    		private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			Collection<ClassroomSession> classroomSessions = inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]);
    			ClassroomSession classroomSession = classroomSessions.isEmpty() ? null : classroomSessions.iterator().next();
    			return new Key(classroomSession == null ? (String)getInput()[0]+(String)getInput()[1] : classroomSession.getCode());
    		}
    	});
    	
    	addClass(ClassroomSessionSubject.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<ClassroomSessionSubject>(ClassroomSessionSubject.class){
			private static final long serialVersionUID = 1L;
			@Override
			protected ClassroomSessionSubject __execute__() {
				ClassroomSessionSubject classroomSessionSubject = super.__execute__();
				if(classroomSessionSubject.getClassroomSession()==null)
					return null;
				classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
				return classroomSessionSubject;
			}
    	}.addParameterArrayElementStringIndexInstance(3,ClassroomSessionSubject.FIELD_TEACHER));
    	
    	instanceKeyBuilderMap.put(ClassroomSessionSubject.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE){
    		private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			ClassroomSessionSubject classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class)
    					.findByClassroomSessionBySubject(inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]).iterator().next()
    							, Pool.getInstance().get(Subject.class, getInput()[2]));
    			return new Key(classroomSessionSubject == null ? (String)getInput()[0]+getInput()[1]+getInput()[2] : classroomSessionSubject.getCode());
    		}
    	});
    	
    	addClass(StudentClassroomSession.class,new org.cyk.system.root.business.impl.helper.InstanceHelper.BuilderOneDimensionArray<StudentClassroomSession>(StudentClassroomSession.class){
			private static final long serialVersionUID = 1L;
			@Override
			protected StudentClassroomSession __execute__() {
				String levelCode = null;
				if("P".equals(getInput()[4])){
					if("PK".equals(getInput()[1]))
						levelCode = "K1";
					else if("K1".equals(getInput()[1]))
						levelCode = "K2";
					else if("K2".equals(getInput()[1]))
						levelCode = "K3";
					else if("K3".equals(getInput()[1]))
						levelCode = "G1";
					else {
						Integer index = Integer.parseInt( ((String)getInput()[1]).substring(1) );
						if(index<12)
							levelCode = "G"+(index+1);
						
						//if(!"G11".equals(levelCode))
						//	levelCode = null;
					}
					//if(!"G1".equals(levelCode))
					//	levelCode = null;
				}else{
					levelCode = (String)getInput()[1];
				}
				if(levelCode!=null){
					String suffix = ArrayUtils.contains(new String[]{"G6","G7","G8","G9","G10","G11","G12"}, levelCode) ? null : (String)getInput()[2];
					StudentClassroomSession studentClassroomSession = super.__execute__();
					if(studentClassroomSession.getStudent()==null)
						return null;
					ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix(levelCode,suffix).iterator().next();
					studentClassroomSession.setClassroomSession(classroomSession);
					return studentClassroomSession;
				}else
					return null;
			}
    	}.addParameterArrayElementStringIndexInstance(0,StudentClassroomSession.FIELD_STUDENT));
    	
    	instanceKeyBuilderMap.put(StudentClassroomSession.class, new org.cyk.system.root.business.impl.helper.ArrayHelper.KeyBuilder(Boolean.FALSE){
    		private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			//StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class)
    			//		.findByStudentByClassroomSession(Pool.getInstance().get(Student.class, getInput()[0]),inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[1],(String)getInput()[2]).iterator().next()
    			//				);
    			return new Key(String.valueOf(getInput()[1])+String.valueOf(getInput()[2]));
    		}
    	});
    	*/
    	
	}

	/*
	@Getter @Setter @Accessors(chain=true)
    public static class KeyBuilder extends ArrayHelper.Dimension.Key.Builder.Adapter.Default implements Serializable {
		private static final long serialVersionUID = 1L;
    	
		private Boolean isGlobalIdentifier;
		
		public KeyBuilder(Boolean isGlobalIdentifier) {
			super();
			this.isGlobalIdentifier = isGlobalIdentifier;
		}
		
		public KeyBuilder() {
			this(Boolean.FALSE);
		}
		
		@Override
		protected Key __execute__() {
			return new ArrayHelper.Dimension.Key(Boolean.TRUE.equals(isGlobalIdentifier) 
					? InstanceHelper.Pool.getInstance().get(GlobalIdentifier.class, getInput()[0]).getCode() : (String)getInput()[0]);
		}

    }*/
	
}
