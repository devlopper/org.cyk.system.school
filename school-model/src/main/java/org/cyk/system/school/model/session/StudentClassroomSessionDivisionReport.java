package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.StudentResultsReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.formatter.NumberFormatter;
import org.cyk.utility.common.generator.AbstractGeneratable;
import org.cyk.utility.common.generator.RandomDataProvider;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReport extends AbstractStudentNodeReport<StudentClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private ActorReport student = new ActorReport();
	private ClassroomSessionDivisionReport classroomSessionDivision = new ClassroomSessionDivisionReport();
	
	private String attendedTime,missedTime,missedTimeJustified,averagePromotionScale,totalAverage,totalCoefficient,totalAverageCoefficiented,
		comments,subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	private List<String> markTotals = new ArrayList<>();
	private List<BigDecimal> tempMarkTotals = new ArrayList<>();
	
	private Collection<StudentClassroomSessionDivisionSubjectReport> subjects = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects;
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	
	private ActorReport commentator = new ActorReport();
	
	private List<String> subjectsTableColumnNames = new ArrayList<>();
	
	public StudentClassroomSessionDivisionReport(ClassroomSessionDivisionReport classroomSessionDivision) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
	}
	
	public StudentClassroomSessionDivisionReport(ClassroomSessionDivisionReport classroomSessionDivision,StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
		setSource(studentClassroomSessionDivision);
	}
	
	public StudentResultsReport getResultsByClassroomSessionDivisionSubjectReportAtIndex(Integer index){
		ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = classroomSessionDivision.getClassroomSessionDivisionSubjectAtIndex(index);
		for(StudentClassroomSessionDivisionSubjectReport studentClassroomSessionDivisionSubjectReport : subjects)
			if(studentClassroomSessionDivisionSubjectReport.getClassroomSessionDivisionSubject() == classroomSessionDivisionSubjectReport){
				return studentClassroomSessionDivisionSubjectReport.getResults();
			}
		return NULL_STUDENT_RESULTS_REPORT;
	}
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		StudentClassroomSessionDivision studentClassroomSessionDivision = (StudentClassroomSessionDivision) source;
		student.setSource(studentClassroomSessionDivision.getStudent());
		if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()) 
				&& studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()!=null){
			
			//setAverage(format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()));
			getAverageScale().setSource(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverageAppreciatedInterval());
			
			if(studentClassroomSessionDivision.getResults().getEvaluationSort().getRank().getValue()!=null){
				
				//TODO rank formatting should be moved in RankReport
				NumberFormatter.String numberFormatter = new NumberFormatter.String.Adapter.Default(studentClassroomSessionDivision.getResults().getEvaluationSort().getRank().getValue()
						,null);
				numberFormatter.setIsAppendOrdinalSuffix(Boolean.TRUE);
				numberFormatter.setIsAppendExaequo(studentClassroomSessionDivision.getResults().getEvaluationSort().getRank().getExaequo());
				numberFormatter.setIsOrdinal(Boolean.TRUE);
				numberFormatter.setLocale(AbstractGeneratable.Listener.Adapter.Default.LOCALE);
				getResults().getEvaluationSort().getRank().setValueExaequo(numberFormatter.execute());
				
			}
			
			if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAveragePromotedInterval()!=null)
				setAveragePromotionScale(RootConstant.Code.getRelativeCode(studentClassroomSessionDivision.getResults().getEvaluationSort().getAveragePromotedInterval()));
			
			setTotalCoefficient(format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDivisor()));
			setTotalAverage(format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDividend()));
			setTotalAverageCoefficiented(format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getDividend()));
		}
		
		if(Boolean.TRUE.equals(studentClassroomSessionDivision.getStudentClassroomSessionDivisionSubjects().isSynchonizationEnabled())){
			for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : studentClassroomSessionDivision.getStudentClassroomSessionDivisionSubjects().getCollection()){
				StudentClassroomSessionDivisionSubjectReport studentClassroomSessionDivisionSubjectReport = new StudentClassroomSessionDivisionSubjectReport(this
						, null, studentClassroomSessionDivisionSubject);
				subjects.add(studentClassroomSessionDivisionSubjectReport);
			}
		}
	}
	
	public void setClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects){
		for(StudentClassroomSessionDivisionSubjectReport studentClassroomSessionDivisionSubject : subjects)
			for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
				if( ((StudentClassroomSessionDivisionSubject)studentClassroomSessionDivisionSubject.getSource()).getClassroomSessionDivisionSubject().getCode()
						.equals(classroomSessionDivisionSubject.getCode())){
					studentClassroomSessionDivisionSubject.setClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
					//StudentClassroomSessionDivisionSubject s = (StudentClassroomSessionDivisionSubject) studentClassroomSessionDivisionSubject.getSource();
					//System.out.println("AVG : "+s.getIdentifier()+":"+s.getResults().getEvaluationSort().getAverage().getValue());
					break;
				}
			}
	}
	
	@Override
	public void generate() {
		super.generate();
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
	
		academicSession.getCompany().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		academicSession.getCompany().setGenerateBackground(Boolean.TRUE);
		academicSession.generate();
		
		student.getPerson().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		student.generate();
		
		commentator.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		commentator.generate();
		
		//name = "THIRD TERM PRIMARY REPORT CARD";
		attendedTime = positiveFloatNumber(999, 0, 99);
		missedTime = positiveFloatNumber(999, 0, 99);
		missedTimeJustified = positiveFloatNumber(999, 0, 99); 
		totalAverage = positiveFloatNumber(999, 0, 99);
		totalCoefficient = positiveFloatNumber(999, 0, 99);
		totalAverageCoefficiented = positiveFloatNumber(999, 0, 99);
		comments = provider.randomText(4, 6, 15, 20);
		
		if(classroomSessionDivisionSubjects==null){
			classroomSessionDivisionSubjects = new ArrayList<>();
			for(int i=0;i<18;i++){
				ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject = new ClassroomSessionDivisionSubjectReport();
				classroomSessionDivisionSubject.generate();
				classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
			}
		}
		
		for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			StudentClassroomSessionDivisionSubjectReport subject = new StudentClassroomSessionDivisionSubjectReport(this,classroomSessionDivisionSubject,null);
			subject.generate();
			subjects.add(subject);
		}
		
		for(int i=0;i<3;i++)
			markTotals.add(positiveFloatNumber(20, 0, 99));
	}
	
	public StudentClassroomSessionDivisionReport addSubjectsTableColumnNames(String...names){
		subjectsTableColumnNames.addAll(Arrays.asList(names));
		return this;
	}
	
	public void generateSubjects(Collection<ClassroomSessionDivisionSubjectReport> classroomSessionDivisionSubjects,Boolean skipable){
		subjects = new ArrayList<>();
		Integer count = 0;
		for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport : classroomSessionDivisionSubjects){
			count++;
			if(Boolean.TRUE.equals(skipable) && RandomDataProvider.getInstance().randomInt(1, 3)==2)
				continue;
			StudentClassroomSessionDivisionSubjectReport studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubjectReport(this,classroomSessionDivisionSubjectReport,null);
			studentClassroomSessionDivisionSubject.generate();
			studentClassroomSessionDivisionSubject.getResults().getEvaluationSort().getAverage().setValue(RandomDataProvider.getInstance().randomInt(10, 99)+"."
					+RandomDataProvider.getInstance().randomInt(10, 99));
			studentClassroomSessionDivisionSubject.getResults().getEvaluationSort().getRank().setValue(String.valueOf(RandomDataProvider.getInstance().randomInt(10, 99)));
			subjects.add(studentClassroomSessionDivisionSubject);
		}
		
	}
	
	@Override
	public String toString() {
		return classroomSessionDivision.getName()+" | "+marks;
	}
}