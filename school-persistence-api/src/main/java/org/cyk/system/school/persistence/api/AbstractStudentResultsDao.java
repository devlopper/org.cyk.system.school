package org.cyk.system.school.persistence.api;

import java.util.Collection;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Rank;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.AbstractStudentResult;

public interface AbstractStudentResultsDao<LEVEL extends AbstractIdentifiable,RESULT extends AbstractStudentResult<LEVEL,DETAIL>,DETAIL> extends TypedDao<RESULT> {

	Collection<RESULT> findByRank(LEVEL level,Rank rank);
	
}
