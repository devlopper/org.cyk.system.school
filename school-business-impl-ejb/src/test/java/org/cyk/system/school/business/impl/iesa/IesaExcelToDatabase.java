package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;
import java.util.Collection;

import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.BusinessInterfaceLocator;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.report.InternationalEnglishSchoolOfAbidjanReportProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.utility.common.helper.ArrayHelper;
import org.cyk.utility.common.helper.ArrayHelper.Dimension.Key;
import org.cyk.utility.common.helper.CollectionHelper;
import org.cyk.utility.common.helper.FieldHelper;
import org.cyk.utility.common.helper.InstanceHelper;
import org.cyk.utility.common.helper.MicrosoftExcelHelper;

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
    	createIdentifiable(JobTitle.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER));
    	
    	createIdentifiable(JobTitle.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER));
    	createIdentifiable(Person.class, Boolean.TRUE, FieldHelper.getInstance().buildPath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_IDENTIFIER)
    			,Person.FIELD_LASTNAMES);
    	
    	testCase.assertIdentifiable(JobTitle.class,"SUPERVISOR", "Nursery and Primary Supervisor");
    	testCase.assertIdentifiable(JobTitle.class,"HS DIRECTOR", "High School Director");
    	
    	testCase.assertPerson("IESA/2013TEE0344-KG", "TOKPA", "Edward Elvis");
    	
    	//System.exit(0);
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
    		identifiable.setGlobalIdentifier(new InstanceHelper.Copy.Adapter.Default<GlobalIdentifier>(getGlobalIdentifier(identifiable.getGlobalIdentifier().getIdentifier())).execute());
    		identifiable.getGlobalIdentifier().setIdentifier(null);
    	}
    }
    
    private void setGlobalIdentifier(AbstractIdentifiable identifiable){
    	setGlobalIdentifier(Arrays.asList(identifiable));
    }
    
    private <T extends AbstractIdentifiable> void createIdentifiable(Class<T> aClass,final Boolean globalIdentifier,String...fields){
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
		InstanceHelper.Builder.OneDimensionArray.Adapter.Default<T> instanceBuilder = new InstanceHelper.Builder.OneDimensionArray.Adapter.Default<T>(aClass){
			private static final long serialVersionUID = 1L;
			@Override
			protected T __execute__() {
				T identifiable = super.__execute__();
				if(identifiable.getIdentifier()==null)
					setGlobalIdentifier(identifiable);
				return identifiable;
			}
		};
		instanceBuilder.setKeyBuilder(new ArrayHelper.Dimension.Key.Builder.Adapter.Default(){
			private static final long serialVersionUID = 1L;
			protected ArrayHelper.Dimension.Key __execute__() {
    			return new ArrayHelper.Dimension.Key(getGlobalIdentifier((String)getInput()[0]).getCode());
			}
		}).addParameterArrayElementString(fields);
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
    
    /**/
         
}
