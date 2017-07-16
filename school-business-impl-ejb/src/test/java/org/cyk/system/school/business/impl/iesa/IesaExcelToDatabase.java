package org.cyk.system.school.business.impl.iesa;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.IdentifiableExcelSheetReader;
import org.cyk.system.root.business.impl.InstanciateAdapter;
import org.cyk.system.root.business.impl.OneDimensionObjectArrayAdapter;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.ClassHelper;
import org.cyk.utility.common.helper.FileHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.MicrosoftExcelHelper;

public class IesaExcelToDatabase extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
     
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	AbstractRootReportProducer.DEFAULT = new InternationalEnglishSchoolOfAbidjanReportProducer();    	
    	
    	persons();
    	
    	System.exit(0);
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
        
}
