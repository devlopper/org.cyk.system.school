package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.inject.Inject;

import org.cyk.system.root.business.api.event.EventBusiness;
import org.cyk.system.root.business.api.event.EventMissedBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.time.Attendance;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.root.persistence.api.event.EventMissedDao;
import org.cyk.system.root.persistence.api.event.EventParticipationDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.persistence.api.subject.LectureDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

public abstract class AbstractStudentResultsBusinessImpl<LEVEL extends AbstractIdentifiable,RESULT extends AbstractStudentResult<LEVEL,DETAIL>,DAO extends TypedDao<RESULT>,DETAIL> extends AbstractTypedBusinessService<RESULT,DAO> implements AbstractStudentResultsBusiness<LEVEL,RESULT,DETAIL>,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject protected MathematicsBusiness mathematicsBusiness;
	@Inject protected IntervalBusiness intervalBusiness;
	protected SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
	@Inject protected EventBusiness eventBusiness;
	@Inject protected EventMissedBusiness eventMissedBusiness;
	
	@Inject protected StudentSubjectEvaluationDao evaluatedStudentDao;
	@Inject protected LectureDao lectureDao;
	@Inject protected EventParticipationDao eventParticipationDao;
	@Inject protected EventMissedDao eventMissedDao;
	
	public AbstractStudentResultsBusinessImpl(DAO dao) {
		super(dao); 
	}
	
	@Override
	public void average(Collection<LEVEL> levels,Collection<RESULT> results,Collection<DETAIL> details,Boolean keepDetails) {
		for(LEVEL level : levels){
			Collection<RESULT> lResults = new ArrayList<>();
			for(RESULT result : results)
				if(level(result).equals(level))
					lResults.add(result);
			
			Collection<DETAIL> lDetails = new ArrayList<>();
			for(DETAIL detail : details)
				if(level(detail).equals(level))
					lDetails.add(detail);
				
			average(lResults, lDetails,keepDetails);
		}
	}
	
	@Override
	public void average(Collection<RESULT> results,Collection<DETAIL> details,Boolean keepDetails) {
		logTrace("Computing average in module {} . Results={} Details={} KeepDetails={}",getClazz().getName(), results.size(),details.size(),keepDetails);
		for(RESULT result : results){
			Collection<WeightedValue> weightedValues = new ArrayList<WeightedValue>();
			//filtering of the data belonging to the student
			for(DETAIL detail : details)
				if(result.getStudent().equals(student(detail))){
					result.getDetails().add(detail);
					WeightedValue weightedValue = weightedValue(detail);
					if(weightedValue.getValue()==null)
						;
					else
						weightedValues.add(weightedValue);
				}
			//computation
			//Is there any weighted values
			if(weightedValues.isEmpty()){
				logTrace("No weighted values found for {}.  No average will be computed", result);
			}else{
				
				Average average = mathematicsBusiness.average(weightedValues, schoolBusinessLayer.getAverageComputationListener(), schoolBusinessLayer.getAverageComputationScript());
				//setting
				result.getResults().getEvaluationSort().setAverage(average); 
				result.getResults().getEvaluationSort().setAverageInterval(intervalBusiness.findByCollectionByValue(averageIntervalCollection(level(result)),average.getValue(), 2));
				logTrace("Average {} , Interval {}",result.getResults().getEvaluationSort().getAverage(),result.getResults().getEvaluationSort().getAverageInterval());
			}
		}
		
	}
	
	@Override
	public Collection<RESULT> average(Collection<LEVEL> levels,Boolean keepDetails) {
		//Structure
		Collection<RESULT> results = readResults(levels);
		//Data
		Collection<DETAIL> details = readDetails(levels,keepDetails);
		//Computation
		average(results, details,keepDetails);
		return results;
	}
 
	@Override
	public void rank(Collection<LEVEL> levels, Collection<RESULT> results,RankOptions<SortableStudentResults> options) {
		for(LEVEL level : levels){
			Collection<RESULT> lResults = new ArrayList<>();
			for(RESULT result : results)
				if(level(result).equals(level))
					lResults.add(result);
			rank(lResults,options);
		}
	}
	
	@Override
	public void rank(Collection<RESULT> results,RankOptions<SortableStudentResults> options) {
		List<SortableStudentResults> sortables = new ArrayList<>();
		for(RESULT result : results) 
			sortables.add(new SortableStudentResults(result,Boolean.TRUE));  
		
		mathematicsBusiness.rank(sortables, options);
	}

	@Override
	public void attendance(Collection<LEVEL> levels,Collection<RESULT> results,Collection<Lecture> lectures,Collection<EventParticipation> participations,
			Collection<EventMissed> eventMisseds) {
		for(RESULT result : results){
			Attendance attendance = result.getResults().getLectureAttendance();
			attendance.setAttendedDuration(0l);
			attendance.setMissedDuration(0l);
			attendance.setMissedDurationJustified(0l);
			for(LEVEL level : levels){
				if(level(result).equals(level))
					for(Lecture lecture : lectures)
						if(level(lecture).equals(level))
							for(EventParticipation participation : participations)
								if(participation.getEvent().equals(lecture.getEvent())){
									if(result.getStudent().getPerson().equals(participation.getParty())){
										EventMissed lEventMissed = null;
										for(EventMissed eventMissed : eventMisseds)
											if(eventMissed.getParticipation().equals(participation)){
												lEventMissed = eventMissed;
												break;
											} 
										if(lEventMissed==null){
											attendance.addAttendedDuration(participation.getEvent().getPeriod().getDuration());
										}else{
											attendance.addAttendedDuration(participation.getEvent().getPeriod().getDuration()-lEventMissed.getDuration());
											attendance.addMissedDuration(lEventMissed.getDuration());
											if(lEventMissed.getReason()!=null && Boolean.TRUE.equals(lEventMissed.getReason().getAcceptable()))
												attendance.addMissedDurationJustified(lEventMissed.getDuration());
										}
									}
							}	
			}
		}
	}
	
	@Override
	public Collection<RESULT> attendance(Collection<LEVEL> levels) {
		Collection<RESULT> results = readResults(levels);
		Collection<Lecture> lectures = readLectures(levels);
		Collection<Event> events = lectureDao.readEvents(lectures);
		Collection<EventParticipation> eventParticipations = eventParticipationDao.readByEvents(events);
		Collection<EventMissed> eventMisseds = eventMissedDao.readByEventParticipations(eventParticipations);
		
		attendance(levels,results, lectures,eventParticipations, eventMisseds);
		return results;
	}
	
	/**/
	
	protected abstract WeightedValue weightedValue(DETAIL detail);

	protected abstract Student student(DETAIL detail);
	
	protected abstract Collection<RESULT> readResults(Collection<LEVEL> levels);
	
	protected abstract Collection<Lecture> readLectures(Collection<LEVEL> levels);
	
	protected abstract Collection<DETAIL> readDetails(Collection<LEVEL> levels,Boolean keepDetails);
	
	protected abstract LEVEL level(DETAIL detail);

	protected abstract LEVEL level(RESULT result);
	
	protected abstract LEVEL level(Lecture lecture);
	
	protected abstract IntervalCollection averageIntervalCollection(LEVEL level);
}
