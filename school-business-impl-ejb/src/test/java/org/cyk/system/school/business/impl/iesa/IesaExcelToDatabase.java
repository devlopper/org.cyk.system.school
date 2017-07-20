package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.BusinessInterfaceLocator;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobInformations;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonExtendedInformations;
import org.cyk.system.root.model.party.person.PersonRelationship;
import org.cyk.system.root.model.party.person.PersonRelationshipExtremity;
import org.cyk.system.root.model.party.person.PersonRelationshipTypeRole;
import org.cyk.system.root.model.party.person.Sex;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;
import org.cyk.utility.common.helper.CollectionHelper;
import org.cyk.utility.common.helper.FieldHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.InstanceHelper.Pool;
import org.cyk.utility.common.helper.MicrosoftExcelHelper;
import org.cyk.utility.common.helper.StringHelper;

public class IesaExcelToDatabase extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    private String workbookFileName = System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa\\IESA_2017_2016.xlsx";
    private static Collection<GlobalIdentifier> globalIdentifiers;
    
    @Override
    protected void businesses() {
    	TestCase testCase = instanciateTestCase();
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	loadGlobalIdentifiers();
    	
    	createIdentifiable(JobTitle.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER));
    	
    	Pool.getInstance().load(Sex.class);//TODO should work without this line
    	createIdentifiable(Person.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			,Person.FIELD_LASTNAMES,Person.FIELD_SURNAME,FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER
    					,GlobalIdentifier.FIELD_BIRTH_LOCATION,AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_OTHER_DETAILS),Person.FIELD_NATIONALITY
    					,Person.FIELD_SEX
    			,FieldHelper.getInstance().buildPath(Person.FIELD_EXTENDED_INFORMATIONS,PersonExtendedInformations.FIELD_TITLE)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_FUNCTION)
    			,FieldHelper.getInstance().buildPath(Person.FIELD_JOB_INFORMATIONS,JobInformations.FIELD_TITLE));
    	
    	Pool.getInstance().load(Person.class);//TODO should work without this line
    	createIdentifiable(Student.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_CODE),Student.FIELD_PERSON);
    	
    	Pool.getInstance().load(Student.class);//TODO should work without this line
    	Pool.getInstance().load(PersonRelationshipTypeRole.class);//TODO should work without this line
    	InstanceHelperBuilderOneDimensionArrayAdapterDefault<PersonRelationship> instanceBuilder = new InstanceHelperBuilderOneDimensionArrayAdapterDefault<PersonRelationship>(PersonRelationship.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			protected PersonRelationship __execute__() {
				PersonRelationship personRelationship = super.__execute__();
				Boolean person1IsStudent = Pool.getInstance().get(Student.class, getInput()[0])!=null;
				
				PersonRelationshipExtremity parentExtremity,studentExtremity;
				String studentExtremityRole,parentExtremityRole = "FAMILYFATHER".equals(getInput()[2]) ? RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_FATHER
						: RootConstant.Code.PersonRelationshipTypeRole.FAMILY_PARENT_MOTHER;
				if(Boolean.TRUE.equals(person1IsStudent)){
					studentExtremity = personRelationship.getExtremity1();
					parentExtremity = personRelationship.getExtremity2();
				}else{
					studentExtremity = personRelationship.getExtremity2();
					parentExtremity = personRelationship.getExtremity1();
				}
				if(studentExtremity.getPerson().getSex()==null)
					System.out.println(studentExtremity.getPerson().getCode());
				
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
    	createIdentifiable(PersonRelationship.class, Boolean.FALSE, instanceBuilder);
    	
    	testCase.assertIdentifiable(JobTitle.class,"SUPERVISOR", "Nursery and Primary Supervisor");
    	testCase.assertIdentifiable(JobTitle.class,"HS DIRECTOR", "High School Director");
    	
    	testCase.assertPerson("IESA/2013TEE0344-KG", "TOKPA", "Edward Elvis",RootConstant.Code.Sex.MALE,"19/05/2009","Monrovia, Liberia");
    	
    	//System.exit(0);
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
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier>(GlobalIdentifier.class);
		instanceBuilder.addParameterArrayElementString(GlobalIdentifier.FIELD_IDENTIFIER,GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME
				,GlobalIdentifier.FIELD_DESCRIPTION,GlobalIdentifier.FIELD_ABBREVIATION,GlobalIdentifier.FIELD_OTHER_DETAILS,GlobalIdentifier.FIELD_EXTERNAL_IDENTIFIER,GlobalIdentifier.FIELD_ORDER_NUMBER
				,GlobalIdentifier.FIELD_WEIGHT,GlobalIdentifier.FIELD_USABLE,FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_EXISTENCE_PERIOD,Period.FIELD_FROM_DATE)
				);
		/*instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
				return new ArrayHelper.Dimension.Key((String)getInput()[0]);
			}
		});*/
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		globalIdentifiers = instancesBuilder.execute();
		//System.out.println(StringHelper.getInstance().get(ArrayHelper.getInstance().filter(globalIdentifiers, 0, "Person_1490728652829_XPTlGB4XJO"),"\r\n","|"));
    }
    
    private static GlobalIdentifier getGlobalIdentifier(String identifier){
    	return CollectionHelper.getInstance().getFirst(new CollectionHelper.Filter.Adapter.Default<GlobalIdentifier>(globalIdentifiers)
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
    	createIdentifiable(aClass, globalIdentifier, instanceBuilder);
    }
    
    private <T extends AbstractIdentifiable> void createIdentifiable(Class<T> aClass,final Boolean globalIdentifier,InstanceHelper.Builder.OneDimensionArray<T> instanceBuilder){
    	InstanceHelper.Pool.getInstance().load(aClass);
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,aClass);    	
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1);
		builder.getMatrix().getRow().setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			return new ArrayHelper.Dimension.Key(Boolean.TRUE.equals(globalIdentifier) ? getGlobalIdentifier((String)getInput()[0]).getCode() : (String)getInput()[0]);
    		}
    	});
    	builder.getMatrix().getRow().getKeyBuilder().addParameters(new Object[]{0});
    	builder.getMatrix().getRow().addIgnoredKeyValues(InstanceHelper.getInstance().callGetMethod(InstanceHelper.Pool.getInstance().get(aClass), String.class
    			, GlobalIdentifier.FIELD_CODE));	
    	
    	MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T>();
		
		instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
    			return new ArrayHelper.Dimension.Key(Boolean.TRUE.equals(globalIdentifier) ? getGlobalIdentifier((String)getInput()[0]).getCode() : (String)getInput()[0]);
			}
		});
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		inject(BusinessInterfaceLocator.class).injectTyped(aClass).synchronize(sheet,instanceBuilder);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer()
    			.setStructurationEnabled(Boolean.FALSE)
    			.setSynchronizationEnabled(Boolean.FALSE)
    			.setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getDivisionOrderNumbers().clear();
    	return dataProducer;
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
    
    /**/
         
}
