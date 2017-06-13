package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.api.session.LevelBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.AbstractPrimefacesPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class RankListPage extends AbstractPrimefacesPage implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentClassroomSessionDetails> table;
	private List<StudentClassroomSessionDetails> list = new ArrayList<>();
	
	@Override
	protected void initialisation() {
		super.initialisation();
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(StudentClassroomSession studentClassroomSession : inject(StudentClassroomSessionBusiness.class).findAll())
			if(studentClassroomSession.getResults().getEvaluationSort()!=null && studentClassroomSession.getResults().getEvaluationSort().getRank()!=null
					&& studentClassroomSession.getResults().getEvaluationSort().getRank().getValue()!=null)
				studentClassroomSessions.add(studentClassroomSession);
		
		int rank;
		Comparator<StudentClassroomSession> comparator = new Comparator<StudentClassroomSession>() {
			@Override
			public int compare(StudentClassroomSession o1, StudentClassroomSession o2) {
				if(o1.getResults().getEvaluationSort().getAverage()==null)
					if(o2.getResults().getEvaluationSort().getAverage()==null)
						return 0;
					else
						return -1;
				else
					if(o2.getResults().getEvaluationSort().getAverage()==null)
						return 1;
					else
						return o2.getResults().getEvaluationSort().getAverage().getValue().compareTo(o1.getResults().getEvaluationSort().getAverage().getValue());
			}
		};
		
		for(LevelGroup levelGroup : inject(LevelGroupBusiness.class).findAll()){
			List<StudentClassroomSession> levelGroupStudentClassroomSessions = new ArrayList<>();
			for(StudentClassroomSession studentClassroomSession : studentClassroomSessions){
				if(studentClassroomSession.getClassroomSession().getLevelTimeDivision().getLevel().getGroup().equals(levelGroup))
					levelGroupStudentClassroomSessions.add(studentClassroomSession);
			}
			Collections.sort(levelGroupStudentClassroomSessions, comparator);
			
			rank = 0;
			for(StudentClassroomSession levelGroupStudentClassroomSession : levelGroupStudentClassroomSessions)
				for(StudentClassroomSession studentClassroomSession : studentClassroomSessions)
					if(studentClassroomSession.equals(levelGroupStudentClassroomSession)){
						studentClassroomSession.setRankGroup(++rank);
						break;
					}
		}
		
		for(Level level : inject(LevelBusiness.class).findAll()){
			List<StudentClassroomSession> levelStudentClassroomSessions = new ArrayList<>();
			for(StudentClassroomSession studentClassroomSession : studentClassroomSessions){
				if(studentClassroomSession.getClassroomSession().getLevelTimeDivision().getLevel().equals(level))
					levelStudentClassroomSessions.add(studentClassroomSession);
			}
			
			Collections.sort(levelStudentClassroomSessions, comparator);
			
			rank = 0;
			for(StudentClassroomSession levelStudentClassroomSession : levelStudentClassroomSessions)
				for(StudentClassroomSession studentClassroomSession : studentClassroomSessions)
					if(studentClassroomSession.equals(levelStudentClassroomSession)){
						studentClassroomSession.setRankLevel(++rank);
						break;
					}
		}
		
		for(StudentClassroomSession studentClassroomSession : studentClassroomSessions){
			StudentClassroomSessionDetails studentClassroomSessionDetails = new StudentClassroomSessionDetails(studentClassroomSession);
			list.add(studentClassroomSessionDetails);
		}
	}
	
}
