package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.session.EvaluationTypeBusiness;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;

public class EvaluationTypeBusinessImpl extends AbstractEnumerationBusinessImpl<EvaluationType, EvaluationTypeDao> implements EvaluationTypeBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public EvaluationTypeBusinessImpl(EvaluationTypeDao dao) {
		super(dao); 
	}   
	
}
