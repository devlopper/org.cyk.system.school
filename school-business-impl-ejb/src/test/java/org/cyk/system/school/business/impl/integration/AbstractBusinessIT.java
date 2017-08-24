package org.cyk.system.school.business.impl.integration;

import java.math.BigDecimal;
import java.util.Collection;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.transaction.UserTransaction;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyBusinessTestHelper;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.party.ApplicationBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.BusinessIntegrationTestHelper;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.RootBusinessTestHelper;
import org.cyk.system.root.business.impl.RootDataProducerHelper;
import org.cyk.system.root.business.impl.validation.DefaultValidator;
import org.cyk.system.root.business.impl.validation.ExceptionUtils;
import org.cyk.system.root.business.impl.validation.ValidatorMap;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.root.persistence.api.message.SmtpPropertiesDao;
import org.cyk.system.root.persistence.impl.GenericDaoImpl;
import org.cyk.system.root.persistence.impl.PersistenceIntegrationTestHelper;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl._test.SchoolBusinessTestHelper;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;
import org.cyk.utility.common.test.TestEnvironmentListener;
import org.cyk.utility.test.ArchiveBuilder;
import org.cyk.utility.test.integration.AbstractIntegrationTestJpaBased;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.junit.Assert;

public abstract class AbstractBusinessIT extends AbstractIntegrationTestJpaBased {

	static {
		TestEnvironmentListener.COLLECTION.add(new TestEnvironmentListener.Adapter.Default(){

			private static final long serialVersionUID = 1L;
			@Override
    		public void assertEquals(String message, Object expected, Object actual) {
    			Assert.assertEquals(message, expected, actual);
    		}
    		@Override
    		public String formatBigDecimal(BigDecimal value) {
    			return inject(NumberBusiness.class).format(value);
    		}
    	});
	}
	
	@Deployment
    public static Archive<?> createDeployment() {
    	Archive<?> archive = createRootDeployment();
    	return archive;
    }
	
	private static final long serialVersionUID = -5752455124275831171L;
	@Inject protected ExceptionUtils exceptionUtils; 
	@Inject protected DefaultValidator defaultValidator;
	@Inject protected GenericDaoImpl g;
	@Inject protected GenericBusiness genericBusiness;
	@Inject protected ApplicationBusiness applicationBusiness;
	@Inject protected TeacherBusiness teacherBusiness;
	@Inject protected RootDataProducerHelper rootDataProducerHelper;
	
	protected UserAccount userAccount;;
	
	@Inject protected ValidatorMap validatorMap;// = ValidatorMap.getInstance();
	@Inject protected RootBusinessLayer rootBusinessLayer;
	//@Inject protected RootBusinessTestHelper rootTestHelper;
	@Inject protected CompanyBusinessLayer companyBusinessLayer;
	@Inject protected CompanyBusinessTestHelper companyBusinessTestHelper;
	@Inject protected SchoolBusinessLayer schoolBusinessLayer;
	@Inject protected SchoolBusinessTestHelper schoolBusinessTestHelper;
	
    @Inject protected UserTransaction userTransaction;
	
    protected AcademicSession academicSession;
    
    protected TestCase instanciateTestCase(){
		return schoolBusinessTestHelper.instanciateTestCase();
	}
    
    @Override
    public EntityManager getEntityManager() {
        return g.getEntityManager();
    }
	
    @Override
    protected void populate() {
    	installApplication();
    }
    
    @Override
    protected Boolean populateInTransaction() {
    	return Boolean.FALSE;
    }

    protected void installApplication(){
    	schoolBusinessLayer.installApplication();
		RootBusinessLayer.getInstance().setDefaultSmtpProperties(inject(SmtpPropertiesDao.class).read(RootConstant.Code.SmtpProperties.DEFAULT));
    	academicSession = inject(AcademicSessionDao.class).readOneRandomly();
    }
    
	@Override
    protected void _execute_() {
        super._execute_();
        create();    
        read(); 
        update();    
        delete();    
        finds();
        businesses();
    }
    
	protected void finds(){}
	
	protected void businesses(){}
	
	/* Shortcut */
    
    protected AbstractIdentifiable create(AbstractIdentifiable object){
        return genericBusiness.create(object);
    }
    
    @SuppressWarnings("unchecked")
	protected <T extends AbstractIdentifiable> void create(Collection<T> objects){
        genericBusiness.create((Collection<AbstractIdentifiable>) objects);
    }
    
    protected AbstractIdentifiable update(AbstractIdentifiable object){
        return genericBusiness.update(object);
    }
    
    @SuppressWarnings("unchecked")
	protected <T extends AbstractIdentifiable> void update(Collection<T> objects){
        genericBusiness.update((Collection<AbstractIdentifiable>) objects);
    }
        
    public static Archive<?> createRootDeployment() {
        return  
                new ArchiveBuilder().create().getArchive().
                    addClasses(BusinessIntegrationTestHelper.classes()).
                    addClasses(PersistenceIntegrationTestHelper.classes()).
                    addClasses(RootBusinessLayer.class,RootBusinessTestHelper.class,CompanyBusinessLayer.class).
                    addPackages(Boolean.FALSE, BusinessIntegrationTestHelper.packages()).
                    addPackages(Boolean.TRUE,"org.cyk.system.company").
                    addPackages(Boolean.TRUE,"org.cyk.system.school") 
                ;
    } 
    
    @Override protected void create() {}
    @Override protected void delete() {}
    @Override protected void read() {}
    @Override protected void update() {}

}
