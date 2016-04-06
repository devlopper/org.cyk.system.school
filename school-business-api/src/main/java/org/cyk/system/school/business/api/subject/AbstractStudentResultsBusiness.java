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
	void average(Collection<LEVEL> levels,Collection<RESULT> results,Collection<DETAIL> details,Boolean keepDetails);
	
	/**
	 * Based on the details , compute the average
	 */
	void average(Collection<RESULT> types,Collection<DETAIL> details,Boolean keepDetails);
	
	/**
	 * Compute the average of a level
	 */
	Collection<RESULT> average(Collection<LEVEL> levels,Boolean keepDetails);
	
	/* Rank computation */
	
	void rank(Collection<LEVEL> levels,Collection<RESULT> results,RankOptions<SortableStudentResults> options);
	
	void rank(Collection<RESULT> results,RankOptions<SortableStudentResults> options);

	Collection<RESULT> updateRank(Collection<LEVEL> levels,RankOptions<SortableStudentResults> options);
	
	void attendance(Collection<LEVEL> levels,Collection<RESULT> results, Collection<Lecture> lectures,Collection<EventParticipation> participations,Collection<EventMissed> eventMisseds);
	
	//void attendance(Collection<LEVEL> levels,Collection<RESULT> results);
	
	Collection<RESULT> attendance(Collection<LEVEL> levels);
	
	/* Attendance computation */
	
	/**
	 * Based on the details , compute the attendance by grouping by level
	 */
	//void attendance(Collection<LEVEL> levels,Collection<RESULT> results,Collection<DETAIL> details,Boolean keepDetails);
	
	/**
	 * Based on the details , compute the attendance
	 */
	//void attendance(Collection<RESULT> types,Collection<DETAIL> details,Boolean keepDetails);
}
