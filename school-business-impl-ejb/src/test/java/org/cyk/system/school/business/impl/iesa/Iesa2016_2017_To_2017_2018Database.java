package org.cyk.system.school.business.impl.iesa;

import java.io.FileInputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.impl.BusinessInterfaceLocator;
import org.cyk.system.root.business.impl.DataSet;
import org.cyk.system.root.business.impl.globalidentification.GlobalIdentifierBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;
import org.cyk.utility.common.helper.CollectionHelper;
import org.cyk.utility.common.helper.FieldHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.InstanceHelper.Pool;
import org.cyk.utility.common.helper.MethodHelper;
import org.cyk.utility.common.helper.MicrosoftExcelHelper;
import org.cyk.utility.common.helper.StringHelper;
import org.cyk.utility.common.helper.SystemHelper;
import org.cyk.utility.common.helper.TimeHelper;

public class Iesa2016_2017_To_2017_2018Database extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    private String workbookFileName = System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa\\IESA_2017_2016.xlsx";
    private String images = StringHelper.getInstance().appendIfDoesNotEndWith(SystemHelper.getInstance().getProperty("images.directory.path"),"\\");
    private Map<Class<?>,Integer> identifiableCountMap = new HashMap<>();
    private static Collection<GlobalIdentifier> GLOBAL_IDENTIFIERS;
   
    @Override
    protected void businesses() {
    	GlobalIdentifierBusinessImpl.BuilderOneDimensionArray.IMAGE_DIRECTORY_PATH = images;
    	Iesa_2016_2017_DataSet dataSet = new Iesa_2016_2017_DataSet(); 	
    	dataSet.instanciate();
    	dataSet.create();
    	
    	/*
    	identifiableCountMap.put(Person.class, SystemHelper.getInstance().getPropertyAs(Integer.class,"personCount"));
    	TestCase testCase = instanciateTestCase();
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	loadGlobalIdentifiers();
    	academicSession = inject(AcademicSessionBusiness.class).findDefaulted();
    	createIdentifiable(JobTitle.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER));
    	
    	Pool.getInstance().load(Sex.class);//TODO should work without this line
    	
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<Person> personInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<Person>(Person.class){
					private static final long serialVersionUID = 1L;
					@Override
					protected Person __execute__() {
						Person person = super.__execute__();
						person.setContactCollection(new ContactCollection());
						
						if(person.getExtendedInformations().getSignatureSpecimen()!=null && !StringHelper.getInstance().isBlank(person.getExtendedInformations().getSignatureSpecimen().getUri())){
							String name = person.getExtendedInformations().getSignatureSpecimen().getUri()+"."+person.getExtendedInformations().getSignatureSpecimen().getExtension();
							String path = images+name;
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
						return person;
					}
    		
    	};
    	personInstanceBuilder.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			,Person.FIELD_LASTNAMES,Person.FIELD_SURNAME,FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER
    					,GlobalIdentifier.FIELD_BIRTH_LOCATION,AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_OTHER_DETAILS),Person.FIELD_NATIONALITY
    					,Person.FIELD_SEX
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_TITLE)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_FUNCTION)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_TITLE)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_SIGNATURE_SPECIMEN,File.FIELD_URI)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_SIGNATURE_SPECIMEN,File.FIELD_EXTENSION));
    	
    	createIdentifiable(Person.class, personInstanceBuilder , new ArrayHelperDimensionKeyBuilder(Boolean.TRUE));
    	
    	Pool.getInstance().load(Person.class);//TODO should work without this line
    	
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<UserAccount> userAccountInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<UserAccount>(UserAccount.class){
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
    		
    	};
    	userAccountInstanceBuilder.addParameterArrayElementStringIndexInstance(0,FieldHelper.getInstance().buildPath(UserAccount.FIELD_CREDENTIALS,Credentials.FIELD_USERNAME)
    			,1,FieldHelper.getInstance().buildPath(UserAccount.FIELD_CREDENTIALS,Credentials.FIELD_PASSWORD));
    	createIdentifiable(UserAccount.class, userAccountInstanceBuilder,new ArrayHelperDimensionKeyBuilder());
    	
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<Student> studentInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<Student>(Student.class){
					private static final long serialVersionUID = 1L;
					@Override
					protected Student __execute__() {
						Student student = super.__execute__();
						if(student.getPerson()==null)
							return null;
						student.setImage(student.getPerson().getImage());
						return student;
					}
    		
    	};
    	studentInstanceBuilder.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_CODE),Student.FIELD_PERSON);
    	
    	createIdentifiable(Student.class, studentInstanceBuilder , new  ArrayHelperDimensionKeyBuilder(Boolean.TRUE));
    	
    	Pool.getInstance().load(Student.class);//TODO should work without this line
    	Pool.getInstance().load(PersonRelationshipTypeRole.class);//TODO should work without this line
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<PersonRelationship> instanceBuilder = new InstanceHelperBuilderOneDimensionArrayAdapterDefault<PersonRelationship>(PersonRelationship.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			protected PersonRelationship __execute__() {
				PersonRelationship personRelationship = super.__execute__();
				if(personRelationship.getExtremity1().getPerson()==null || personRelationship.getExtremity2().getPerson()==null)
					return null;
				Boolean person1IsStudent = Pool.getInstance().get(Student.class, getInput()[0])!=null;
				
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
				
				studentExtremity.setRole(Pool.getInstance().get(PersonRelationshipTypeRole.class, studentExtremityRole));
				parentExtremity.setRole(Pool.getInstance().get(PersonRelationshipTypeRole.class, parentExtremityRole));
				
				return personRelationship;
			}
    	};
    	instanceBuilder.addParameterArrayElementStringIndexInstance(0,FieldHelper.getInstance().buildPath(PersonRelationship.FIELD_EXTREMITY_1,PersonRelationshipExtremity.FIELD_PERSON)
    	    	,2,FieldHelper.getInstance().buildPath(PersonRelationship.FIELD_EXTREMITY_2,PersonRelationshipExtremity.FIELD_PERSON));
    	createIdentifiable(PersonRelationship.class, instanceBuilder,new ArrayHelperDimensionKeyBuilder());
    	
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<Teacher> teacherInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<Teacher>(Teacher.class){
					private static final long serialVersionUID = 1L;
					@Override
					protected Teacher __execute__() {
						Teacher teacher = super.__execute__();
						if(teacher.getPerson()==null)
							return null;
						return teacher;
					}
    		
    	};
    	teacherInstanceBuilder.addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_CODE),Student.FIELD_PERSON);
    	
    	createIdentifiable(Teacher.class, teacherInstanceBuilder , new  ArrayHelperDimensionKeyBuilder(Boolean.TRUE));
    	Pool.getInstance().load(Teacher.class);//TODO should work without this line
    	
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<ElectronicMail> electronicMailInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<ElectronicMail>(ElectronicMail.class){
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
    		
    	};
    	electronicMailInstanceBuilder.addParameterArrayElementStringIndexInstance(0,ElectronicMail.FIELD_ADDRESS);
    	createIdentifiable(ElectronicMail.class, electronicMailInstanceBuilder,new ArrayHelperDimensionKeyBuilder());
    	
    	classroomSessions();
    	*/
    	System.exit(0);
    }
    
    private void loadGlobalIdentifiers(){
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,GlobalIdentifier.class);
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1);
		MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		for(Object[] array : sheet.getValues()){
			if(!StringHelper.getInstance().isBlank((String)array[10])){
				String date = (String)array[10];
				String[] p = StringUtils.split(StringUtils.substringBefore(date, Constant.CHARACTER_SPACE.toString()),"-");
				array[10] = p[2]+"/"+p[1]+"/"+p[0];
			}
		}
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<GlobalIdentifier> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<GlobalIdentifier>(sheet.getValues());
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier>(GlobalIdentifier.class){
			private static final long serialVersionUID = 1L;
			@Override
			protected GlobalIdentifier __execute__() {
				GlobalIdentifier globalIdentifier = super.__execute__();
				if(globalIdentifier.getImage()!=null && !StringHelper.getInstance().isBlank(globalIdentifier.getImage().getUri())){
					String name = globalIdentifier.getImage().getUri()+"."+globalIdentifier.getImage().getExtension();
					String path = images+name;
					byte[] bytes;
					try {
						bytes = IOUtils.toByteArray(new FileInputStream(path));
						globalIdentifier.setImage(inject(FileBusiness.class).process(bytes, name));
					} catch (Exception e) {
						e.printStackTrace();
					}
				}else if(globalIdentifier.getImage()!=null){
					globalIdentifier.setImage(null);
				}
				return globalIdentifier;
			}
		};
		instanceBuilder.addParameterArrayElementString(GlobalIdentifier.FIELD_IDENTIFIER,GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME
				,GlobalIdentifier.FIELD_DESCRIPTION,GlobalIdentifier.FIELD_ABBREVIATION,GlobalIdentifier.FIELD_OTHER_DETAILS,GlobalIdentifier.FIELD_EXTERNAL_IDENTIFIER,GlobalIdentifier.FIELD_ORDER_NUMBER
				,GlobalIdentifier.FIELD_WEIGHT,GlobalIdentifier.FIELD_USABLE,FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_EXISTENCE_PERIOD,Period.FIELD_FROM_DATE)
				,FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_EXISTENCE_PERIOD,Period.FIELD_TO_DATE)
				,FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_IMAGE,File.FIELD_URI),FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_IMAGE,File.FIELD_EXTENSION));
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		GLOBAL_IDENTIFIERS = instancesBuilder.execute();
    }
    
    private static GlobalIdentifier getGlobalIdentifier(String identifier){
    	return CollectionHelper.getInstance().getFirst(new CollectionHelper.Filter.Adapter.Default<GlobalIdentifier>(GLOBAL_IDENTIFIERS)
    		.setProperty(CollectionHelper.Filter.PROPERTY_NAME_FIELD_NAME, GlobalIdentifier.FIELD_IDENTIFIER)
    		.setProperty(CollectionHelper.Filter.PROPERTY_NAME_FIELD_VALUE, identifier)
    		.execute());
    }
    
    private static void setGlobalIdentifier(Collection<AbstractIdentifiable> identifiables){
    	for(AbstractIdentifiable identifiable : identifiables){
    		identifiable.setGlobalIdentifier(new InstanceHelper.Copy.Adapter.Default<GlobalIdentifier>(getGlobalIdentifier(identifiable.getGlobalIdentifier().getIdentifier())).execute());
    		identifiable.getGlobalIdentifier().setIdentifier(null);
    	}
    }
    
    private static void setGlobalIdentifier(AbstractIdentifiable identifiable){
    	if(identifiable.getGlobalIdentifier()==null)
    		return;
    	setGlobalIdentifier(Arrays.asList(identifiable));
    }
    
    private <T extends AbstractIdentifiable> void createIdentifiable(Class<T> aClass,final Boolean globalIdentifier,String...fields){
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<T> instanceBuilder = new InstanceHelperBuilderOneDimensionArrayAdapterDefault<T>(aClass);
    	instanceBuilder.addParameterArrayElementString(fields);
    	ArrayHelper.Dimension.Key.Builder keyBuilder = new ArrayHelperDimensionKeyBuilder(globalIdentifier);
    	createIdentifiable(aClass, instanceBuilder,keyBuilder);
    	
    }
    
    private <T extends AbstractIdentifiable> void createIdentifiable(Class<T> aClass,InstanceHelper.Builder.OneDimensionArray<T> instanceBuilder,ArrayHelper.Dimension.Key.Builder keyBuilder){
    	Long millisecond = System.currentTimeMillis();
    	System.out.print(aClass.getSimpleName()+" ");
    	InstanceHelper.Pool.getInstance().load(aClass);
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,aClass);    	
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1);
    	builder.getMatrix().getRow().setNumberOfIndexes(identifiableCountMap.get(aClass));
    	builder.getMatrix().getRow().setKeyBuilder(keyBuilder);
    	builder.getMatrix().getRow().getKeyBuilder().addParameters(new Object[]{0});
    	builder.getMatrix().getRow().addIgnoredKeyValues(MethodHelper.getInstance().callGet(InstanceHelper.Pool.getInstance().get(aClass), String.class
    			, GlobalIdentifier.FIELD_CODE));	
    	
    	System.out.print("sheet ");
    	MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
    	System.out.print("(#values="+ArrayHelper.getInstance().size(sheet.getValues())+",#ignored="+ArrayHelper.getInstance().size(sheet.getIgnoreds())+") ");
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T>();
		instanceBuilder.setKeyBuilder(keyBuilder);
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		System.out.print("synchronise ");
		inject(BusinessInterfaceLocator.class).injectTyped(aClass).synchronize(sheet,instanceBuilder);
		System.out.println("SUCCESS. "+new TimeHelper.Stringifier.Duration.Adapter.Default(System.currentTimeMillis()-millisecond).execute());
    }
    
    private void classroomSessions(){
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<ClassroomSession> classroomSessionInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<ClassroomSession>(ClassroomSession.class);
    	classroomSessionInstanceBuilder.addParameterArrayElementStringIndexInstance(2,ClassroomSession.FIELD_COORDINATOR);
    	Pool.getInstance().load(LevelTimeDivision.class);
    	Pool.getInstance().load(ClassroomSessionSuffix.class);
    	createIdentifiable(ClassroomSession.class, classroomSessionInstanceBuilder,new ArrayHelperDimensionKeyBuilder(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			//System.out.println(getInput()[0]+" - "+getInput()[1]+" : "+inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]));
    			Collection<ClassroomSession> classroomSessions = inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]);
    			ClassroomSession classroomSession = classroomSessions.isEmpty() ? null : classroomSessions.iterator().next();
    			return new Key(classroomSession == null ? (String)getInput()[0]+(String)getInput()[1] : classroomSession.getCode());
    		}
    	});
    	
    	
    	Pool.getInstance().load(ClassroomSession.class);
    	Pool.getInstance().load(Student.class);
    	/*
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<StudentClassroomSession> studentClassroomSessionInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<StudentClassroomSession>(StudentClassroomSession.class){
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
							}
						}else
							levelCode = (String)getInput()[1];
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
    		
    	};
    	
    	studentClassroomSessionInstanceBuilder.addParameterArrayElementStringIndexInstance(0,StudentClassroomSession.FIELD_STUDENT);
    	createIdentifiable(StudentClassroomSession.class, studentClassroomSessionInstanceBuilder,new ArrayHelperDimensionKeyBuilder(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class)
    					.findByStudentByClassroomSession(Pool.getInstance().get(Student.class, getInput()[0]),inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[1],(String)getInput()[2]).iterator().next()
    							);
    			return new Key(String.valueOf(getInput()[1])+String.valueOf(getInput()[2]));
    		}
    	});
    	*/
    	
    	Pool.getInstance().load(Subject.class);
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<ClassroomSessionSubject> classroomSessionSubjectInstanceBuilder =
    			new InstanceHelperBuilderOneDimensionArrayAdapterDefault<ClassroomSessionSubject>(ClassroomSessionSubject.class){
					private static final long serialVersionUID = 1L;
					@Override
					protected ClassroomSessionSubject __execute__() {
						ClassroomSessionSubject classroomSessionSubject = super.__execute__();
						if(classroomSessionSubject.getClassroomSession()==null)
							return null;
						classroomSessionSubject.setCascadeOperationToChildren(Boolean.TRUE);
						return classroomSessionSubject;
					}
    		
    	};
    	classroomSessionSubjectInstanceBuilder.addParameterArrayElementStringIndexInstance(3,ClassroomSessionSubject.FIELD_TEACHER);
    	createIdentifiable(ClassroomSessionSubject.class, classroomSessionSubjectInstanceBuilder,new ArrayHelperDimensionKeyBuilder(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			//System.out.println(getInput()[0]+" - "+getInput()[1]+" : "+inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]));
    			ClassroomSessionSubject classroomSessionSubject = inject(ClassroomSessionSubjectBusiness.class)
    					.findByClassroomSessionBySubject(inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix((String)getInput()[0],(String)getInput()[1]).iterator().next()
    							, Pool.getInstance().get(Subject.class, getInput()[2]));
    			return new Key(classroomSessionSubject == null ? (String)getInput()[0]+getInput()[1]+getInput()[2] : classroomSessionSubject.getCode());
    		}
    	});
    	
    }
    
    public static class InstanceHelperBuilderOneDimensionArrayAdapterDefault<T extends AbstractIdentifiable> extends InstanceHelper.Builder.OneDimensionArray.Adapter.Default<T> implements Serializable{
    	private static final long serialVersionUID = 1L;
    	
    	public InstanceHelperBuilderOneDimensionArrayAdapterDefault(Class<T> outputClass) {
			super(outputClass);
		}
		
		@Override
		protected T __execute__() {
			T identifiable = super.__execute__();
			if(identifiable.getIdentifier()==null){
				GlobalIdentifier globalIdentifier1 = identifiable.getGlobalIdentifier();
				setGlobalIdentifier(identifiable);
				GlobalIdentifier globalIdentifier2 = identifiable.getGlobalIdentifier();
				if(globalIdentifier1!=null){
					globalIdentifier2.setBirthLocation(globalIdentifier1.getBirthLocation());
				}
			}
			return identifiable;
		}
	}
    
    @Getter @Setter @Accessors(chain=true)
    public static class ArrayHelperDimensionKeyBuilder extends ArrayHelper.Dimension.Key.Builder.Adapter.Default implements Serializable {
		private static final long serialVersionUID = 1L;
    	
		private Boolean isGlobalIdentifier;
		
		public ArrayHelperDimensionKeyBuilder(Boolean isGlobalIdentifier) {
			super();
			this.isGlobalIdentifier = isGlobalIdentifier;
		}
		
		public ArrayHelperDimensionKeyBuilder() {
			this(Boolean.FALSE);
		}
		
		@Override
		protected Key __execute__() {
			return new ArrayHelper.Dimension.Key(Boolean.TRUE.equals(isGlobalIdentifier) ? getGlobalIdentifier((String)getInput()[0]).getCode() : (String)getInput()[0]);
		}

    }
    
    /**/
         
}
