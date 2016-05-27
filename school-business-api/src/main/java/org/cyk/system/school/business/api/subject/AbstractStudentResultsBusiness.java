package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.subject.Lecture;

public interface AbstractStudentResultsBusiness<LEVEL extends AbstractIdentifiable,RESULT extends AbstractStudentResult<LEVEL,DETAIL>,DETAIL> extends TypedBusiness<RESULT> {
	
	/* Average computation */
	
	/**
	 * Based on the details , compute the average by grouping by level
	 */
	void updateAverage(Collection<LEVEL> levels,Collection<RESULT> results,Collection<DETAIL> details,BusinessServiceCallArguments<RESULT> callArguments);
	
	/**
	 * Based on the details , compute the average
	 */
	void updateAverage(Collection<RESULT> types,Collection<DETAIL> details,BusinessServiceCallArguments<RESULT> callArguments);
	
	/**
	 * Compute the average of a level
	 */
	Collection<RESULT> updateAverage(Collection<LEVEL> levels,BusinessServiceCallArguments<RESULT> callArguments);
	
	/* Rank computation */
	
	void updateRank(Collection<LEVEL> levels,Collection<RESULT> results,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments);
	
	void rank(Collection<RESULT> results,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments);

	Collection<RESULT> updateRank(Collection<LEVEL> levels,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments);
	
	/* Attendance computation */
	
	void updateAttendance(Collection<LEVEL> levels,Collection<RESULT> results, Collection<Lecture> lectures,Collection<EventParticipation> participations,Collection<EventMissed> eventMisseds,BusinessServiceCallArguments<RESULT> callArguments);
	
	Collection<RESULT> updateAttendance(Collection<LEVEL> levels,BusinessServiceCallArguments<RESULT> callArguments);
	
	/**/
	
	Collection<RESULT> updateResults(Collection<LEVEL> levels,Boolean updateEvaluationAverage,Boolean updateRank,RankOptions<SortableStudentResults> rankOptions,Boolean updateAttendance,BusinessServiceCallArguments<RESULT> callArguments);

}
