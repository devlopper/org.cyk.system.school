package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
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
import org.cyk.system.root.model.event.EventParty;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.time.Attendance;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.root.persistence.api.event.EventMissedDao;
import org.cyk.system.root.persistence.api.event.EventPartyDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.persistence.api.subject.LectureDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

public abstract class AbstractStudentResultsBusinessImpl<LEVEL extends AbstractIdentifiable,RESULT extends AbstractStudentResult<LEVEL,DETAIL>,DAO extends TypedDao<RESULT>,DETAIL> extends AbstractTypedBusinessService<RESULT,DAO> implements AbstractStudentResultsBusiness<LEVEL,RESULT,DETAIL>,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject protected MathematicsBusiness mathematicsBusiness;
	@Inject protected IntervalBusiness intervalBusiness;
	protected SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
	@Inject protected EventBusiness eventBusiness;
	@Inject protected EventMissedBusiness eventMissedBusiness;
	
	@Inject protected StudentClassroomSessionDivisionSubjectEvaluationDao evaluatedStudentDao;
	@Inject protected LectureDao lectureDao;
	@Inject protected EventPartyDao eventPartyDao;
	@Inject protected EventMissedDao eventMissedDao;
	
	public AbstractStudentResultsBusinessImpl(DAO dao) {
		super(dao); 
	}

	@Override
	protected Object[] getPropertyValueTokens(RESULT result, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{result.getStudent()};
		return super.getPropertyValueTokens(result, name);
	}
	
	@Override
	public RESULT create(RESULT identifiable) {
		if(identifiable.getResults()==null){
			identifiable.setResults(new StudentResults());
		}
		return super.create(identifiable);
	}
	
	@Override
	public void updateAverage(Collection<LEVEL> levels,Collection<RESULT> results,Collection<DETAIL> details,BusinessServiceCallArguments<RESULT> callArguments) {
		for(LEVEL level : levels){
			Collection<RESULT> lResults = new ArrayList<>();
			for(RESULT result : results)
				if(level(result).equals(level))
					lResults.add(result);
			
			Collection<DETAIL> lDetails = new ArrayList<>();
			for(DETAIL detail : details)
				if(level(detail).equals(level))
					lDetails.add(detail);
				
			updateAverage(lResults, lDetails,callArguments);
		}
	}
	
	@Override
	public void updateAverage(Collection<RESULT> results,Collection<DETAIL> details,BusinessServiceCallArguments<RESULT> callArguments) {
		logTrace("Computing average in module {} . {}={} {}={}",getClazz().getSimpleName(),getResultClass().getSimpleName() ,results.size()
				,getDetailsClass().getSimpleName(),details.size());
		for(RESULT result : results){
			setCallArgumentsCurrentExecutionStep(callArguments, result);
			
			Collection<WeightedValue> weightedValues = new ArrayList<WeightedValue>();
			//filtering of the data belonging to the student
			result.getDetails().clear();
			for(DETAIL detail : details){
				//System.out.println(result.getStudent().getIdentifier()+" , "+student(detail).getIdentifier()+" : "+result.getStudent().getIdentifier().equals(student(detail).getIdentifier()));
				if(result.getStudent().getIdentifier().equals(student(detail).getIdentifier())){
					result.getDetails().add(detail);
					WeightedValue weightedValue = weightedValue(detail);
					//System.out.println(result.getStudent().getCode()+" , "+weightedValue.getValue());
					if(weightedValue.getValue()==null)
						;
					else{
						weightedValues.add(weightedValue);
						//System.out.println(result.getStudent().getCode()+" , "+weightedValue.getValue()+" , "+weightedValue.getWeight());
					}
				}
			}
			//computation
			//Is there any weighted values
			if(weightedValues.isEmpty()){
				logTrace("No {} found so no average will be computed", getDetailsClass().getSimpleName());
				//logTrace("No weighted values found for {}.  No average will be computed", result);
			}else{
				Average average = mathematicsBusiness.average(weightedValues, schoolBusinessLayer.getAverageComputationListener(), schoolBusinessLayer.getAverageComputationScript());
				//debug(result.getResults());
				//setting
				result.getResults().getEvaluationSort().setAverage(average); 
				result.getResults().getEvaluationSort().setAverageAppreciatedInterval(intervalBusiness.findByCollectionByValue(averageAppreciatedIntervalCollection(level(result)),average.getValue(), 2));
				result.getResults().getEvaluationSort().setAveragePromotedInterval(intervalBusiness.findByCollectionByValue(averagePromotedIntervalCollection(level(result)),average.getValue(), 2));
				logIdentifiable("Average computed", result);
				//logTrace("Average {} , Interval {}",result.getResults().getEvaluationSort().getAverage(),result.getResults().getEvaluationSort().getAverageInterval());
			}
			dao.update(result);
			addCallArgumentsWorkDoneByStep(callArguments);
		}
		
	}
	
	@Override
	public Collection<RESULT> updateAverage(Collection<LEVEL> levels,BusinessServiceCallArguments<RESULT> callArguments) {
		//Structure
		Collection<RESULT> results = readResults(levels);
		//Data
		Collection<DETAIL> details = readDetails(levels,Boolean.FALSE);
		//Computation
		updateAverage(results, details,callArguments);
		return results;
	}
 
	@Override
	public void updateRank(Collection<LEVEL> levels, Collection<RESULT> results,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments) {
		for(LEVEL level : levels){
			Collection<RESULT> lResults = new ArrayList<>();
			for(RESULT result : results)
				if(level(result).equals(level))
					lResults.add(result);
			rank(lResults,options,callArguments);
		}
		for(RESULT result : results)
			dao.update(result);
	}
	
	@Override
	public void rank(Collection<RESULT> results,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments) {
		List<SortableStudentResults> sortables = new ArrayList<>();
		for(RESULT result : results) {
			sortables.add(new SortableStudentResults(result,Boolean.TRUE));  
		}
		mathematicsBusiness.rank(sortables, options);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public void computeRank(Collection<RESULT> results, RankOptions<SortableStudentResults> options) {
		List<SortableStudentResults> sortables = new ArrayList<>();
		for(RESULT result : results) {
			sortables.add(new SortableStudentResults(result,Boolean.TRUE));  
		}
		mathematicsBusiness.rank(sortables, options);
	}

	@Override
	public Collection<RESULT> updateRank(Collection<LEVEL> levels,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<RESULT> callArguments) {
		Collection<RESULT> results = readResults(levels);
		updateRank(levels, results, options,callArguments);
		return results;
	}
	
	@Override
	public void updateAttendance(Collection<LEVEL> levels,Collection<RESULT> results,Collection<Lecture> lectures,Collection<EventParty> participations,
			Collection<EventMissed> eventMisseds,BusinessServiceCallArguments<RESULT> callArguments) {
		for(RESULT result : results){
			setCallArgumentsCurrentExecutionStep(callArguments, result);
			Attendance attendance = result.getResults().getLectureAttendance();
			if(Boolean.TRUE.equals(isLectureAttendanceAggregatable(result))){
				//Initialize for computation
				attendance.setAttendedDuration(0l);
				attendance.setMissedDuration(0l);
				attendance.setMissedDurationJustified(0l);
				//Aggregate
				for(LEVEL level : levels){
					if(level(result).equals(level))
						for(Lecture lecture : lectures)
							if(level(lecture).equals(level))
								for(EventParty participation : participations)
									if(participation.getEvent().equals(lecture.getEvent())){
										if(result.getStudent().getPerson().equals(participation.getParty())){
											EventMissed lEventMissed = null;
											for(EventMissed eventMissed : eventMisseds)
												if(eventMissed.getEventParty().equals(participation)){
													lEventMissed = eventMissed;
													break;
												} 
											if(lEventMissed==null){
												attendance.addAttendedDuration(participation.getEvent().getExistencePeriod().getNumberOfMillisecond());
											}else{
												attendance.addAttendedDuration(participation.getEvent().getExistencePeriod().getNumberOfMillisecond()-lEventMissed.getNumberOfMillisecond());
												attendance.addMissedDuration(lEventMissed.getNumberOfMillisecond());
												if(lEventMissed.getReason()!=null && Boolean.TRUE.equals(lEventMissed.getReason().getAcceptable()))
													attendance.addMissedDurationJustified(lEventMissed.getNumberOfMillisecond());
											}
										}
								}	
				}	
			}else{
				//Just recompute derived attributes
				Long attendableDuration = getAttendableDuration(result);
				if(attendableDuration==null)
					logTrace("No attendable duration has been set for {}", result.getLogMessage());
				else if(attendance.getMissedDuration()==null)
					logTrace("No missed duration has been set for {}", result.getLogMessage());
				else
					attendance.setAttendedDuration(attendableDuration-attendance.getMissedDuration());
			}
			dao.update(result);
			addCallArgumentsWorkDoneByStep(callArguments);
		}
	}
	
	@Override
	public Collection<RESULT> updateAttendance(Collection<LEVEL> levels,BusinessServiceCallArguments<RESULT> callArguments) {
		//TODO find a way to filter only level where DAO is needed
		Collection<RESULT> results = readResults(levels);
		Collection<Lecture> lectures = readLectures(levels);
		Collection<Event> events = lectureDao.readEvents(lectures);
		Collection<EventParty> eventParties = eventPartyDao.readByEvents(events);
		Collection<EventMissed> eventMisseds = eventMissedDao.readByEventParties(eventParties);
		
		setCallArgumentsObjects(callArguments, results);
		
		updateAttendance(levels,results, lectures,eventParties, eventMisseds,callArguments);
		return results;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.REQUIRED)
	public Collection<RESULT> updateResults(Collection<LEVEL> levels,Boolean updateEvaluationAverage,Boolean updateRank,RankOptions<SortableStudentResults> rankOptions,Boolean updateAttendance,BusinessServiceCallArguments<RESULT> callArguments) {
		Collection<RESULT> results = null;
		if(Boolean.TRUE.equals(updateEvaluationAverage))
			results = updateAverage(levels, callArguments);
		if(Boolean.TRUE.equals(updateAttendance))
			results = updateAttendance(levels, callArguments);
		if(Boolean.TRUE.equals(updateRank))
			results = updateRank(levels, rankOptions, callArguments);
			
		return results;
	}
	
	/**/
	
	protected abstract Class<DETAIL> getDetailsClass();
	protected abstract Class<RESULT> getResultClass();
	
	protected abstract WeightedValue weightedValue(DETAIL detail);

	protected abstract Student student(DETAIL detail);
	
	protected abstract Collection<RESULT> readResults(Collection<LEVEL> levels);
	
	protected abstract Collection<Lecture> readLectures(Collection<LEVEL> levels);
	
	protected abstract Collection<DETAIL> readDetails(Collection<LEVEL> levels,Boolean keepDetails);
	
	protected abstract LEVEL level(DETAIL detail);

	protected abstract LEVEL level(RESULT result);
	
	protected abstract Boolean isLectureAttendanceAggregatable(RESULT result);
	
	protected abstract Long getAttendableDuration(RESULT result);
	
	protected abstract LEVEL level(Lecture lecture);
	
	protected abstract IntervalCollection averageAppreciatedIntervalCollection(LEVEL level);
	
	protected abstract IntervalCollection averagePromotedIntervalCollection(LEVEL level);
}
