package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
	public Collection<Subject> instanciateMany(List<String[]> values) {
		Collection<Subject> subjects = new ArrayList<>();
		for(String[] value : values)
			subjects.add(new Subject(value[0], value[2], null));
		return subjects;
	}
}
