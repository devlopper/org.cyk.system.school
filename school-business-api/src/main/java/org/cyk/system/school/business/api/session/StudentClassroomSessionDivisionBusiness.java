package org.cyk.system.school.business.api.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.utility.common.cdi.BeanAdapter;
import org.cyk.utility.common.computation.ExecutionProgress;

public interface StudentClassroomSessionDivisionBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivision,StudentClassroomSessionDivision,StudentSubject> {

	void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision,BuildReportArguments options);
	void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,BuildReportArguments options);
	void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	
	Collection<File> findReportFiles(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	
	StudentClassroomSessionDivision findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
	//StudentClassroomSessionDivision prepareUpdateOfMetricValues(StudentClassroomSessionDivision studentClassroomSessionDivision);
	StudentClassroomSessionDivision update(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues);
	
	// TODO some methods here can go up
	
	@Getter @Setter @NoArgsConstructor
	public static class BuildReportArguments implements Serializable{
		private static final long serialVersionUID = 7151479991050865862L;
		public static Boolean ATTENDANCE = Boolean.TRUE;
		
		private Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions;
		private Boolean attendance = ATTENDANCE;
		private ExecutionProgress executionProgress; //= new ExecutionProgress("Build Student Classroom Session Division Report", new Double(0));
		
		public BuildReportArguments(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions){
			setStudentClassroomSessionDivisions(studentClassroomSessionDivisions);
		}
		
		public void setStudentClassroomSessionDivisions(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions){
			this.studentClassroomSessionDivisions = studentClassroomSessionDivisions;
			if(this.studentClassroomSessionDivisions != null)
				executionProgress.setTotalAmountOfWork(new Double(studentClassroomSessionDivisions.size()));
		}

	}
	
	//BuildReportArguments DEFAULT_BUILD_REPORT_OPTIONS = new BuildReportArguments();

	Collection<StudentClassroomSessionDivision> findByStudentByClassroomSession(Student student,ClassroomSession classroomSession);
	
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisionIndex(Byte classroomSessionDivisionIndex);

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
}
