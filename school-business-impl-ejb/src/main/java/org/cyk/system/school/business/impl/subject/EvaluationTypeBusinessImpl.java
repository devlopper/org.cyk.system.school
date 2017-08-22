package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.subject.EvaluationTypeBusiness;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;

public class EvaluationTypeBusinessImpl extends AbstractEnumerationBusinessImpl<EvaluationType, EvaluationTypeDao> implements EvaluationTypeBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public EvaluationTypeBusinessImpl(EvaluationTypeDao dao) {
		super(dao); 
	}
	
	@Override
	public EvaluationType instanciateOne(String[] values) {
		EvaluationType evaluationType = super.instanciateOne(values);
		Integer index = 10;
		evaluationType.setMaximum(commonUtils.getBigDecimal(values[index++]));
		return evaluationType;
	}
	
	public static class BuilderOneDimensionArray extends AbstractEnumerationBusinessImpl.BuilderOneDimensionArray<EvaluationType> implements Serializable {
		private static final long serialVersionUID = 1L;

		public BuilderOneDimensionArray() {
			super(EvaluationType.class);
			addParameterArrayElementString(10, EvaluationType.FIELD_MAXIMUM);
		}
		
	}
}
