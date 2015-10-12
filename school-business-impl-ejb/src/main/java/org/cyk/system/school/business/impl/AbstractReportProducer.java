package org.cyk.system.school.business.impl;

import java.io.Serializable;

import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.api.session.ReportProducer;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentSubject;

public abstract class AbstractReportProducer extends org.cyk.system.root.business.impl.AbstractReportProducer implements ReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		StudentClassroomSessionDivisionReport r = new StudentClassroomSessionDivisionReport();
		Student student = studentClassroomSessionDivision.getStudent();
		StudentClassroomSessionDivision s = studentClassroomSessionDivision;
		ClassroomSessionDivision csd = s.getClassroomSessionDivision();
		ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
		AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
		NodeResults results = csd.getResults();
		
		r.getAcademicSession().setFromDateToDate(timeBusiness.formatPeriodFromTo(as.getPeriod()));
		r.getAcademicSession().getCompany().setImage(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(as.getSchool().getOwnedCompany().getCompany().getImage()));
		
		r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		
		r.getClassroomSessionDivision().setName(cs.getUiString());
		r.getClassroomSessionDivision().setAverage(results.getAverage().toString());
		r.getClassroomSessionDivision().setHighestAverage(results.getAverageHighest().toString());
		r.getClassroomSessionDivision().setLowestAverage(results.getAverageLowest().toString());
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		
		r.getStudent().getPerson().setBirthDate(timeBusiness.formatDate(student.getPerson().getBirthDate()));
		r.getStudent().getPerson().setName(student.getPerson().getName());
		r.getStudent().getPerson().setLastName(student.getPerson().getLastName());
		r.getStudent().setRegistrationCode(student.getRegistration().getCode());
		if(student.getPerson().getImage()==null)
			;
		else
			r.getStudent().getPerson().setImage(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(student.getPerson().getImage()));
		
		r.setComments(s.getResults().getAppreciation());
		r.setAverage(s.getResults().getEvaluationSort().getAverage().getValue().toString());
		r.setRank(RootBusinessLayer.getInstance().getMathematicsBusiness().format(s.getResults().getEvaluationSort().getRank()));
		
		r.getAcademicSession().getCompany().setName(as.getSchool().getOwnedCompany().getCompany().getName());
		
		r.getSigner().getPerson().setSignatureSpecimen(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(as.getStaffPersonSigner().getExtendedInformations().getSignatureSpecimen()));
		
		r.setName(languageBusiness.findText("school.report.studentclassroomsessiondivision.title",new Object[]{csd.getUiString()}));
		
		r.setTotalCoefficient(s.getResults().getEvaluationSort().getAverage().getDivisor().toString());
		r.setTotalAverageCoefficiented(s.getResults().getEvaluationSort().getAverage().getDividend().toString());
		
		r.setTotalMissedTime((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		r.setTotalMissedTimeJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
		for(StudentSubject studentSubject : s.getDetails()){
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubjectReport.setAverage("");
			classroomSessionDivisionSubjectReport.setCoefficient(RootBusinessLayer.getInstance().getNumberBusiness()
					.format(studentSubject.getClassroomSessionDivisionSubject().getCoefficient()));
			classroomSessionDivisionSubjectReport.setHighestAverage("");
			classroomSessionDivisionSubjectReport.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			classroomSessionDivisionSubjectReport.setNumberOfStudents("");
			
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport(r,classroomSessionDivisionSubjectReport);
			/*sr.getMarks().add("");
			sr.getMarks().add("");
			sr.getMarks().add("");*/
			//sr.getAverageScale().setCode(r.get); setReport(r);
			/*sr.setAppreciation(studentSubject.getResults().getAppreciation());
			sr.setAverage(studentSubject.getResults().getEvaluationSort().getAverage().getValue().toString());
			sr.setCoefficient(studentSubject.getClassroomSessionDivisionSubject().getCoefficient().toString());
			sr.setAverageCoefficiented(studentSubject.getResults().getEvaluationSort().getAverage().getValue().multiply(studentSubject.getClassroomSessionDivisionSubject().getCoefficient()).toString());
			sr.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			sr.setRank(mathematicsBusiness.format(studentSubject.getResults().getEvaluationSort().getRank()));
			sr.setTeacherNames(studentSubject.getClassroomSessionDivisionSubject().getTeacher().getPerson().getNames());
			*/
			r.getSubjects().add(sr);
		}
		
		
		/*
		comments = provider.randomWord(10, 30);
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
	
		academicSession.getCompany().setGenerateImage(Boolean.TRUE);
		academicSession.generate();
		academicSession.getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
				+ " OF <style forecolor=\"red\">A</style>BIDJAN");
		
		student.getPerson().setGenerateImage(Boolean.TRUE);
		student.generate();
		signer.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		signer.generate();
		
		commentator.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		commentator.generate();
		
		classroomSessionDivision.generate();
		
		name = "THIRD TERM PRIMARY REPORT CARD";
		totalMissedTime = positiveFloatNumber(999, 0, 99);
		totalMissedTimeJustified = positiveFloatNumber(999, 0, 99); 
		totalAverage = positiveFloatNumber(999, 0, 99);
		totalCoefficient = positiveFloatNumber(999, 0, 99);
		totalAverageCoefficiented = positiveFloatNumber(999, 0, 99);
		
		if(classroomSessionDivisionSubjects==null){
			classroomSessionDivisionSubjects = new ArrayList<>();
			for(int i=0;i<15;i++){
				ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject = new ClassroomSessionDivisionSubjectReport();
				classroomSessionDivisionSubject.generate();
				classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
			}
		}
		
		for(ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			StudentClassroomSessionDivisionSubjectReport subject = new StudentClassroomSessionDivisionSubjectReport(this,classroomSessionDivisionSubject);
			subject.generate();
			subjects.add(subject);
		}
		*/
		
		r.setStudentLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.student"));
		labelValue("school.report.studentclassroomsessiondivision.block.student.names", student.getPerson().getNames());
		labelValue("school.report.studentclassroomsessiondivision.block.student.surname", student.getPerson().getSurname());
		labelValue("school.report.studentclassroomsessiondivision.block.student.birthdate", format(student.getPerson().getBirthDate()));
		labelValue("school.report.studentclassroomsessiondivision.block.student.birthlocation", student.getPerson().getExtendedInformations()==null?"":
				student.getPerson().getExtendedInformations().getBirthLocation().toString());
		labelValue("school.report.studentclassroomsessiondivision.block.student.registrationcode", student.getRegistration().getCode());
		labelValue("school.report.studentclassroomsessiondivision.block.student.classroomsessionname", studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().toString());
		labelValue("school.report.studentclassroomsessiondivision.block.student.sex", student.getPerson().getSex()==null?"":student.getPerson().getSex().toString());
		
		r.setAttendanceLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.attendance"));
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.opened", "???");
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.present", "???");
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.absent", "???");
		
		r.setOverallResultlLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.overallresult"));
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.average", "???");
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.grade", "???");
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.rank", "???");
		
		
		/*
		behaviorLabelValueCollection = new LabelValueCollectionReport();
		behaviorLabelValueCollection.setName("BEHAVIOUR,STUDY AND WORK HABITS");
		behaviorLabelValueCollection.add("Respects authority", "4");
		behaviorLabelValueCollection.add("Works independently and neatly", "2");
		behaviorLabelValueCollection.add("Completes homework and class work on time", "3");
		behaviorLabelValueCollection.add("Shows social courtesies", "4");
		behaviorLabelValueCollection.add("Demonstrates self-control", "3");
		behaviorLabelValueCollection.add("Takes care of school and others materials", "2");
		behaviorLabelValueCollection.add("Game/Sport", "4");
		behaviorLabelValueCollection.add("Handwriting", "3");
		behaviorLabelValueCollection.add("Drawing/Painting", "4");
		behaviorLabelValueCollection.add("Punctuality/Regularity", "4");
		behaviorLabelValueCollection.add("Works cooperatively in groups", "2");
		behaviorLabelValueCollection.add("Listens and follows directions", "2");
		
		gradingScaleLabelValueCollection = new LabelValueCollectionReport();
		gradingScaleLabelValueCollection.setName("GRADING SCALE");
		gradingScaleLabelValueCollection.add("A+", "90 - 100 Excellent");
		gradingScaleLabelValueCollection.add("A",  "80 - 89  Very Good");
		gradingScaleLabelValueCollection.add("B+", "70 - 79  Good");
		gradingScaleLabelValueCollection.add("B",  "60 - 69  Fair");
		gradingScaleLabelValueCollection.add("C+", "55 - 59  Satisfactory");
		gradingScaleLabelValueCollection.add("C",  "50 - 54  Barely satisfactory");
		gradingScaleLabelValueCollection.add("E",  " 0 - 49  Fail");
		
		effortLevelLabelValueCollection = new LabelValueCollectionReport();
		effortLevelLabelValueCollection.setName("EFFORT LEVELS");
		effortLevelLabelValueCollection.add("1", "Has no regard for the observable traits");
		effortLevelLabelValueCollection.add("2", "Shows minimal regard for the observable traits");
		effortLevelLabelValueCollection.add("3", "Acceptable level of observable traits");
		effortLevelLabelValueCollection.add("4", "Maintains high level of observable traits");
		effortLevelLabelValueCollection.add("5", "Maintains an excellent degree of observable traits");
		
		informationLabelValueCollection = new LabelValueCollectionReport();
		informationLabelValueCollection.setName("HOME/SCHOOL COMMUNICATIONS");
		informationLabelValueCollection.add("ANNUAL AVERAGE","90");
		informationLabelValueCollection.add("ANNUAL GRADE","B+");
		informationLabelValueCollection.add("ANNUAL RANK","25");
		informationLabelValueCollection.add("PROMOTION INFORMATION","PROMOTED");
		informationLabelValueCollection.add("NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015");
		*/
	
		return r;
	}

}
