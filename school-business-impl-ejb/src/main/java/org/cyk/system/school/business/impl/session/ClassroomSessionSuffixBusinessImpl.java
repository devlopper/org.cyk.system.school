package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.session.ClassroomSessionSuffixBusiness;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSuffixDao;

public class ClassroomSessionSuffixBusinessImpl extends AbstractEnumerationBusinessImpl<ClassroomSessionSuffix, ClassroomSessionSuffixDao> implements ClassroomSessionSuffixBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionSuffixBusinessImpl(ClassroomSessionSuffixDao dao) {
		super(dao); 
	}   
	
	public static class BuilderOneDimensionArray extends AbstractEnumerationBusinessImpl.BuilderOneDimensionArray<ClassroomSessionSuffix> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(ClassroomSessionSuffix.class);
		}
		
	}
	
}
