package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.subject.SubjectBusiness;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.SubjectDao;

public class SubjectBusinessImpl extends AbstractEnumerationBusinessImpl<Subject, SubjectDao> implements SubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public SubjectBusinessImpl(SubjectDao dao) {
		super(dao); 
	}
	
	@Override
	public Subject instanciateOne(String[] values) {
		Subject subject = instanciateOne();
		Integer index = 0;
		subject.setCode(values[index++]);
		subject.setName(values[index++]);
		return subject;
	}
	
	public static class BuilderOneDimensionArray extends AbstractEnumerationBusinessImpl.BuilderOneDimensionArray<Subject> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(Subject.class);
		}
		
	}
}
