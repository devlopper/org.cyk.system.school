package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.lang.annotation.Annotation;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.globalidentification.GlobalIdentifierPersistenceMappingConfiguration;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.utility.common.cdi.AbstractBean;

public class GlobalIdentifierPersistenceMappingConfigurations extends AbstractBean implements Serializable {

	private static final long serialVersionUID = -4261213077562876945L;

	public void configure(){
		GlobalIdentifierPersistenceMappingConfiguration configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		GlobalIdentifierPersistenceMappingConfiguration.Property property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(LevelName.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(ClassroomSession.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(ClassroomSessionDivision.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(ClassroomSessionDivisionSubject.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(ClassroomSessionDivisionSubjectEvaluationType.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(StudentClassroomSession.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(StudentClassroomSessionDivision.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(StudentClassroomSessionDivisionSubject.class, configuration);
        
        configuration = new GlobalIdentifierPersistenceMappingConfiguration();
		property = new GlobalIdentifierPersistenceMappingConfiguration.Property(
				commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE),new javax.persistence.Column() {
			@Override public Class<? extends Annotation> annotationType() {return null;}
			@Override public boolean updatable() {return false;}	
			@Override public boolean unique() {return Boolean.TRUE;}
			@Override public String table() {return null;}
			@Override public int scale() {return 0;}
			@Override public int precision() {return 0;}
			@Override public boolean nullable() {return false;}
			@Override public String name() {return null;}
			@Override public int length() {return 0;}
			@Override public boolean insertable() {return false;}
			@Override public String columnDefinition() {return null;}
		});
        configuration.addProperties(property);
        GlobalIdentifierPersistenceMappingConfiguration.register(StudentClassroomSessionDivisionSubjectEvaluation.class, configuration);
	}
	
}
