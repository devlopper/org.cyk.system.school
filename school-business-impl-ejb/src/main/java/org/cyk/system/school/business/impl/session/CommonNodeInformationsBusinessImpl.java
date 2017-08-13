package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractBusinessServiceImpl;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.CommonNodeInformationsBusiness;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.School;
import org.cyk.utility.common.helper.FieldHelper;

public class CommonNodeInformationsBusinessImpl extends AbstractBusinessServiceImpl implements CommonNodeInformationsBusiness , Serializable {

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("unchecked")
	private <T> T findValue(CommonNodeInformations commonNodeInformations, String fieldName) {
		return (T) FieldHelper.getInstance().read(commonNodeInformations, fieldName);
	}
	
	@Override
	public <T> T findValue(School school,Class<T> aClass, String commonNodeInformationsFieldName) {
		@SuppressWarnings("unchecked")
		T value = (T) findValue(school.getNodeInformations(), commonNodeInformationsFieldName);
		return value;
	}
	
	@Override
	public <T> T findValue(AcademicSession academicSession,Class<T> aClass, String commonNodeInformationsFieldName) {
		@SuppressWarnings("unchecked")
		T value = (T) findValue(academicSession.getNodeInformations(), commonNodeInformationsFieldName);
		if(value==null)
			return findValue(academicSession.getSchool(),aClass, commonNodeInformationsFieldName) ;
		return value;
	}
	
	@Override
	public <T> T findValue(ClassroomSession classroomSession,Class<T> aClass, String commonNodeInformationsFieldName) {
		@SuppressWarnings("unchecked")
		T value = (T) findValue(classroomSession.getNodeInformations(), commonNodeInformationsFieldName);
		if(value==null)
			return findValue(classroomSession.getLevelTimeDivision().getLevel().getLevelName(),aClass, commonNodeInformationsFieldName) ;
		return value;
	}
	
	@Override
	public <T> T findValue(LevelName levelName,Class<T> aClass, String commonNodeInformationsFieldName) {
		@SuppressWarnings("unchecked")
		T value = (T) findValue(levelName.getNodeInformations(), commonNodeInformationsFieldName);
		if(value==null)
			return findValue(inject(AcademicSessionBusiness.class).findDefaulted(),aClass, commonNodeInformationsFieldName) ;
		return value;
	}

	/*public static class GetPropertyCallBackMethod implements org.cyk.utility.common.helper.MethodHelper.Method.CallBack {

		@Override
		public <T> Boolean isExecutable(Object instance, Class<T> resultClass, String name, Parameter[] parameters,T result) {
			return result == null;
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public <T> T execute(Object instance, Class<T> resultClass, String name, Parameter[] parameters, T result) {
			if("getF1".equals(name))
				return (T) MethodHelper.getInstance().call(instance, String.class, "getF11",this);
			if("getF11".equals(name))
				return (T) MethodHelper.getInstance().call(instance, String.class, "getF111",this);
			return null;
		}
	}*/
	
}
