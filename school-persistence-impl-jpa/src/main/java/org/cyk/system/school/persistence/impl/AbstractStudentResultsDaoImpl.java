package org.cyk.system.school.persistence.impl;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Rank;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.persistence.api.AbstractStudentResultsDao;

public abstract class AbstractStudentResultsDaoImpl<LEVEL extends AbstractIdentifiable,RESULT extends AbstractStudentResult<LEVEL,DETAIL>,DETAIL> extends AbstractTypedDao<RESULT> implements AbstractStudentResultsDao<LEVEL,RESULT,DETAIL>,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    protected String findByRank;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        //registerNamedQuery(findByRank, _select().where(getLevelFieldName()).and(commonUtils.attributePath(StudentSubject.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_RANK), Sort.FIELD_RANK, ArithmeticOperator.EQ));
    }
    
    protected abstract String getLevelFieldName();
	
	@Override
	public Collection<RESULT> findByRank(LEVEL level,Rank rank) {
		return namedQuery(findByRank).parameter(getLevelFieldName(), level).parameter(Sort.FIELD_RANK, rank).resultMany();
	}
}
 