package org.cyk.system.school.business.api.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.cdi.BeanAdapter;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

public interface StudentClassroomSessionDivisionBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivision,StudentClassroomSessionDivision,StudentClassroomSessionDivisionSubject> {

	void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision,ServiceCallArguments arguments);
	void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	
	void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean updateEvaluationResults,Boolean updateAttendanceResults,Boolean updateRankResults
			,RankOptions<SortableStudentResults> rankOptions,ServiceCallArguments arguments);
	
	//void computeEvaluationResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,ServiceCallArguments arguments);
	//void computeAttendanceResults(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	
	void setNumberOfTimesAbsent(StudentClassroomSessionDivision studentClassroomSessionDivision,BigDecimal value);
	
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	
	Collection<File> findReportFiles(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	
	StudentClassroomSessionDivision findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
	//StudentClassroomSessionDivision prepareUpdateOfMetricValues(StudentClassroomSessionDivision studentClassroomSessionDivision);
	StudentClassroomSessionDivision update(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues);
	
	// TODO some methods here can go up
	
	Collection<StudentClassroomSessionDivision> findByStudentByClassroomSession(Student student,ClassroomSession classroomSession);
	
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisionIndex(Byte classroomSessionDivisionIndex);

	/**/
	
	@Getter @Setter @NoArgsConstructor
	public static class ServiceCallArguments extends BusinessServiceCallArguments<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = 7151479991050865862L;
		
	}
	
	//TODO to be moved in Impl
	public static interface Listener{
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		void loadedOnBuildReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
		void computingOnBuildReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
		void computedOnBuildReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
		
		/**/
		
		public static class Adapter extends BeanAdapter implements Listener,Serializable{
			private static final long serialVersionUID = -9048282379616583423L;
			@Override
			public void loadedOnBuildReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {}
			
			@Override
			public void computedOnBuildReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {}
			
			@Override
			public void computingOnBuildReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {}
			
		}		
	}

	Collection<StudentClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession);
	Collection<StudentClassroomSessionDivision> findByClassroomSessionByTeacher(ClassroomSession classroomSession,Teacher teacher);
}
