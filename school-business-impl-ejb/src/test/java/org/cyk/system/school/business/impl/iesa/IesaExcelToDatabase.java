package org.cyk.system.school.business.impl.iesa;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.cyk.system.root.business.api.party.person.JobTitleBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.BusinessInterfaceLocator;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.CollectionHelper;
import org.cyk.utility.common.helper.FieldHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.MicrosoftExcelHelper;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;

public class IesaExcelToDatabase extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    private String workbookFileName = System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa\\IESA_2017_2016.xlsx";
    private Collection<GlobalIdentifier> globalIdentifiers;
    
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	TestCase testCase = instanciateTestCase();
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	loadGlobalIdentifiers();
    	createJobTitle();
    	//createPersons();
    	
    	createJobTitle();
    	//persons();
    	
    	testCase.assertIdentifiable(JobTitle.class,"SUPERVISOR", "Nursery and Primary Supervisor");
    	testCase.assertIdentifiable(JobTitle.class,"HS DIRECTOR", "High School Director");
    	
    	testCase.assertPerson("IESA/2013TEE0344-KG", "TOKPA", "Edward Elvis");
    	
    	System.exit(0);
    }
    
    private void loadGlobalIdentifiers(){
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,GlobalIdentifier.class);
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1);
		MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<GlobalIdentifier> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<GlobalIdentifier>(sheet.getValues());
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<GlobalIdentifier>(GlobalIdentifier.class);
		instanceBuilder.addParameterArrayElementStringIndexInstance(0, GlobalIdentifier.FIELD_IDENTIFIER,1,GlobalIdentifier.FIELD_CODE,2,GlobalIdentifier.FIELD_NAME
				/*,3,GlobalIdentifier.FIELD_DESCRIPTION,4,GlobalIdentifier.FIELD_ABBREVIATION,5,GlobalIdentifier.FIELD_OTHER_DETAILS,6,GlobalIdentifier.FIELD_ORDER_NUMBER
				,7,GlobalIdentifier.FIELD_WEIGHT,8,GlobalIdentifier.FIELD_USABLE/*,9,FieldHelper.getInstance().buildPath(GlobalIdentifier.FIELD_EXISTENCE_PERIOD,Period.FIELD_FROM_DATE)*/
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
    
    private GlobalIdentifier getGlobalIdentifier(String identifier){
    	return CollectionHelper.getInstance().getFirst(new CollectionHelper.Filter.Adapter.Default<GlobalIdentifier>(globalIdentifiers)
    		.setProperty(CollectionHelper.Filter.PROPERTY_NAME_FIELD_NAME, GlobalIdentifier.FIELD_IDENTIFIER)
    		.setProperty(CollectionHelper.Filter.PROPERTY_NAME_FIELD_VALUE, identifier)
    		.execute());
    }
    
    private void setGlobalIdentifier(Collection<AbstractIdentifiable> identifiables){
    	for(AbstractIdentifiable identifiable : identifiables){
    		identifiable.setGlobalIdentifier(getGlobalIdentifier(identifiable.getGlobalIdentifier().getIdentifier()));
    		identifiable.getGlobalIdentifier().setIdentifier(null);
    	}
    }
    
    private void setGlobalIdentifier(AbstractIdentifiable identifiable){
    	setGlobalIdentifier(Arrays.asList(identifiable));
    }
    
    private <T extends AbstractIdentifiable> void createIdentifiable(Class<T> aClass,Boolean globalIdentifier,String...fields){
    	InstanceHelper.Pool.getInstance().load(aClass);
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,aClass);    	
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1).createKeyBuilder(new Object[]{0}, InstanceHelper.getInstance()
    			.callGetMethod(InstanceHelper.Pool.getInstance().get(aClass), String.class, GlobalIdentifier.FIELD_CODE));
		MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<T>();
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<T> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<T>(aClass){
			private static final long serialVersionUID = 1L;
			@Override
			protected T __execute__() {
				T identifiable = super.__execute__();
				setGlobalIdentifier(identifiable);
				return identifiable;
			}
		};
		instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
				return new ArrayHelper.Dimension.Key((String)getInput()[0]);
			}
		}).addParameterArrayElementString(fields);
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		inject(BusinessInterfaceLocator.class).injectTyped(aClass).synchronize(sheet,instanceBuilder);
    }
    
    private void createJobTitle(){
    	InstanceHelper.Pool.getInstance().load(JobTitle.class);
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,JobTitle.class);    	
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1);
    	
    	builder.getMatrix().getRow().setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
    		@Override
    		protected Key __execute__() {
    			GlobalIdentifier globalIdentifier = getGlobalIdentifier((String)getInput()[0]);
    			debug(globalIdentifier);
    			return new ArrayHelper.Dimension.Key(globalIdentifier.getCode());
    		}
    	});
    	builder.getMatrix().getRow().getKeyBuilder().addParameters(new Object[]{0});
    	
    	//builder.getMatrix().getRow().createKeyBuilder(new Object[]{0}, InstanceHelper.getInstance()
    	//		.callGetMethod(InstanceHelper.Pool.getInstance().get(JobTitle.class), String.class, GlobalIdentifier.FIELD_CODE));
		
    	MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<JobTitle> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<JobTitle>();
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<JobTitle> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<JobTitle>(JobTitle.class){
			private static final long serialVersionUID = 1L;
			@Override
			protected JobTitle __execute__() {
				JobTitle jobTitle = super.__execute__();
				if(jobTitle.getIdentifier()==null)
					setGlobalIdentifier(jobTitle);
				return jobTitle;
			}
		};
		instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
				System.out
						.println("IesaExcelToDatabase.createJobTitle().new Default() {...}.__execute__() : "+getInput()[0]);
				GlobalIdentifier globalIdentifier = getGlobalIdentifier((String)getInput()[0]);
    			debug(globalIdentifier);
    			return globalIdentifier == null ? null : new ArrayHelper.Dimension.Key(globalIdentifier.getCode());
			}
		}).addParameterArrayElementString(FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER));
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		inject(JobTitleBusiness.class).synchronize(sheet,instanceBuilder);
    }
    
    private void createPersons(){
    	InstanceHelper.Pool.getInstance().load(Person.class);
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter.Default(workbookFileName,Person.class);    	
    	builder.createMatrix().getMatrix().getRow().setFromIndex(1).createKeyBuilder(new Object[]{0}, InstanceHelper.getInstance()
    			.callGetMethod(InstanceHelper.Pool.getInstance().get(Person.class), String.class, GlobalIdentifier.FIELD_CODE));
		MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<Person> instancesBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<Person>();
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<Person> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<Person>(Person.class){
			private static final long serialVersionUID = 1L;
			@Override
			protected Person __execute__() {
				Person person = super.__execute__();
				setGlobalIdentifier(person);
				return person;
			}
		};
		instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
				return new ArrayHelper.Dimension.Key((String)getInput()[0]);
			}
		}).addParameterArrayElementStringIndexInstance(0,FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
				,1,Person.FIELD_LASTNAMES);
		instancesBuilder.setOneDimensionArray(instanceBuilder);
		
		inject(PersonBusiness.class).synchronize(sheet,instanceBuilder);
    }
    
    public void persons(){
    	MicrosoftExcelHelper.Workbook.Sheet.Builder builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter
    			.Default(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa\\2016_2017_Trimester_1.xlsx",0);
    	
    	//builder.createMatrix().getMatrix().getRow().createKeyBuilder(new Object[]{0}, new String[]{"TS/OF/061/2016"});
    	Collection<String> codes = new ArrayList<>();
		for(Person person : inject(PersonDao.class).readAll())
			codes.add(person.getCode());
    	builder.createMatrix().getMatrix().getRow().createKeyBuilder(new Object[]{0}, codes.toArray(new String[]{}));
		MicrosoftExcelHelper.Workbook.Sheet sheet = builder.execute();
		
		InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<Person> instanceBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<Person>(null);
		
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<Person> oneDimensionArray = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<Person>(Person.class);
		oneDimensionArray.setKeyBuilder(
				new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
					private static final long serialVersionUID = 1L;

					protected ArrayHelper.Dimension.Key __execute__() {
						return new ArrayHelper.Dimension.Key((String)getInput()[0]);
					}
				} ).addParameterArrayElementString("globalIdentifier.code");
		instanceBuilder.setOneDimensionArray(oneDimensionArray);
		
		assertEquals(0l, inject(PersonDao.class).countAll());
		
		inject(PersonBusiness.class).synchronize(sheet,new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<Person>(Person.class)
				.addParameterArrayElementString("globalIdentifier.code"));
		
		assertEquals(836l, inject(PersonDao.class).countAll());
		
		/**/
		builder = new MicrosoftExcelHelper.Workbook.Sheet.Builder.Adapter
    			.Default(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa\\2016_2017_Trimester_1.xlsx",0);
    	
		codes = new ArrayList<>();
		for(Person person : inject(PersonDao.class).readAll())
			codes.add(person.getCode());
    	builder.createMatrix().getMatrix().getRow().createKeyBuilder(new Object[]{0}, codes.toArray(new String[]{}));
    	
    	builder.getMatrix().getRow();
		sheet = builder.execute();
		
		instanceBuilder = new InstanceHelper.Builder.TwoDimensionArray.Adapter.Default<Person>(null);
		instanceBuilder.setOneDimensionArray(oneDimensionArray);
		
		assertEquals(836l, inject(PersonDao.class).countAll());
		
		inject(PersonBusiness.class).synchronize(sheet,oneDimensionArray);
		
		assertEquals(836l, inject(PersonDao.class).countAll());
		
		/*
		Collection<Person> createPersons = instanceBuilder.setInput(sheet.getValues()).execute();
		Collection<Person> updatePersons = instanceBuilder.setInput(sheet.getIgnoreds()).execute();
		
		System.out.println(createPersons);
		assertEquals(835, createPersons.size());
		
		System.out.println(updatePersons);
		assertEquals(1, updatePersons.size());
		*/
		/*
		IdentifiableExcelSheetReader<Person> excelSheetReader = new IdentifiableExcelSheetReader<Person>(file,Person.class);
		OneDimensionObjectArrayAdapter<Person> setter = new OneDimensionObjectArrayAdapter<Person>(Person.class);
		
    	org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Person> twoDimensionObjectArray = new org.cyk.utility.common.accessor.InstanceFieldSetter.TwoDimensionObjectArray.Adapter.Default<Person>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key,Object keyType) {
				return Boolean.TRUE;
			}
			
			@Override
			public Person getInstanceByKey(Object[] values, Object key, Object type) {
				Person person = getPerson(values);
				if(values[0].equals("IESA/2014MUJ0441-KG"))
					System.out.println("C : "+values[3]+" : "+person.getBirthDate());
				try {
					String[] p = StringUtils.split((String)values[3], Constant.CHARACTER_SPACE.toString());
					if(p!=null && p.length>0 && StringUtils.isNotBlank(p[0])){
						person.setBirthDate(DateUtils.parseDate(p[0], "MM/dd/yyyy"));
						if(person.getCode().equals("IESA/2014MUJ0441-KG"))
							System.out.println(person.getBirthDate());
						personsUpdated.add(person);
					}
				} catch (ParseException e) {
					System.out.println("Cannot parse : "+values[0]+" : <<"+values[3]+">>");
				}
				return person;
			}
		};
		
		excelSheetReader.execute();
		twoDimensionObjectArray.setInput(excelSheetReader.getValues());
		twoDimensionObjectArray.execute();
		
		System.out.print("Updating persons : "+personsUpdated.size()+"...");
		long t = System.currentTimeMillis();
		update(personsUpdated);
		System.out.println( ((System.currentTimeMillis()-t)/1000) );
		*/
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
    
    /**/
         
}
