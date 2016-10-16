package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;
import java.util.LinkedHashSet;

import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.company.model.sale.Sale;
import org.cyk.system.root.business.api.BusinessService.BusinessServiceCallArguments;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper.ClassroomSessionDivisionInfos;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.EvaluationType;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	inject(StudentBusiness.class).create(
    			inject(StudentBusiness.class).instanciateManyRandomly(new LinkedHashSet<String>(Arrays.asList("STUD1","STUD2","STUD3","STUD4","STUD5"))));
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getPk().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2","STUD3"/*,"STUD4","STUD5"*/},
    			dataProducer.getG1().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getG2().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getG3().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getG4().getClassroomSession(), new Object[][]{{dataProducer.getSubjectsG4G6().size()},{dataProducer.getSubjectsG4G6().size()},{dataProducer.getSubjectsG4G6().size()}}); 
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getG7().getClassroomSession(), new Object[][]{{dataProducer.getSubjectsG7G9().size()},{dataProducer.getSubjectsG7G9().size()},{dataProducer.getSubjectsG7G9().size()}}); 
    	
    	//schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    	//		dataProducer.getG10().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(rootDataProducerHelper.getEnumerations(EvaluationType.class));
    	
    	//trimesterEverybodyHaveAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	//trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	//trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade2().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	//schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getPk().division(0).getClassroomSessionDivision(), null
    	//		, Boolean.TRUE,Boolean.TRUE);
    	
    	ClassroomSessionDivisionInfos classroomSessionDivisionInfos = dataProducer.getG1().division(0);
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(classroomSessionDivisionInfos.getClassroomSessionDivision(), new Object[][]{
    		new Object[]{classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		,{"STUD2","70","50","60"}
    	              ,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	},Boolean.TRUE,Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    	
    	/*StudentClassroomSessionDivision studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class).findByStudentByClassroomSessionDivision(
    			inject(StudentBusiness.class).find("STUD1"), classroomSessionDivisionInfos.getClassroomSessionDivision());
    	CreateReportFileArguments<StudentClassroomSessionDivision> arguments = 
    			new CreateReportFileArguments<StudentClassroomSessionDivision>(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET, studentClassroomSessionDivision);
    	inject(StudentClassroomSessionDivisionBusiness.class).createReportFile(studentClassroomSessionDivision, arguments);
    	*/
    	/*
    	classroomSessionDivisionInfos = dataProducer.getG1().division(1);
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(classroomSessionDivisionInfos.getClassroomSessionDivision(), new Object[][]{
    		new Object[]{classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","55","80","50"}
    	    		,{"STUD2","65","60","60"}
    	              ,{"STUD3","95","25","70"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	},Boolean.TRUE,Boolean.TRUE,Boolean.FALSE,Boolean.TRUE);
    	
    	classroomSessionDivisionInfos = dataProducer.getG1().division(2);
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(classroomSessionDivisionInfos.getClassroomSessionDivision(), new Object[][]{
    		new Object[]{classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","50","30","60"}
    	    		,{"STUD2","50","50","40"}
    	              ,{"STUD3","68","47","83"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	},Boolean.TRUE,Boolean.TRUE,Boolean.FALSE,Boolean.TRUE);
    	*/
    	
    	inject(StudentClassroomSessionBusiness.class).updateAverage(Arrays.asList(dataProducer.getG1().getClassroomSession()), new BusinessServiceCallArguments<StudentClassroomSession>());
    	inject(StudentClassroomSessionBusiness.class).updateRank(Arrays.asList(dataProducer.getG1().getClassroomSession()), 
    			schoolBusinessLayer.getStudentEvaluationResultsRankOptions(), new BusinessServiceCallArguments<StudentClassroomSession>());
    	/*
    	StudentClassroomSession.SearchCriteria searchCriteria = new StudentClassroomSession.SearchCriteria();
    	searchCriteria.getDivisionCount().setLowest(2);
    	searchCriteria.getDivisionCount().setHighest(3);
		searchCriteria.getDivisionIndexesRequired().add(2);
		Collection<StudentClassroomSession> studentClassroomSessions = inject(StudentClassroomSessionBusiness.class).findByCriteria(searchCriteria);
    	System.out.println("SEARCH : "+studentClassroomSessions.size()+" / " +studentClassroomSessions);
    	*/
    	//classroomSessionDivisionInfos = dataProducer.getG1().division(2);
    	//schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(classroomSessionDivisionInfos.getClassroomSessionDivision(), /*new Object[][]{
    	//	new Object[]{classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		//{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	//    	}}
    	//}*/null
    	//,Boolean.TRUE,Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    	
    	//schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(classroomSessionDivisionInfos.getClassroomSessionDivision(), 
    	//		null,Boolean.FALSE,Boolean.FALSE,Boolean.TRUE,Boolean.TRUE);
    	
    	/*
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG4().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG4().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG7().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG7().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG10().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG10().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	*/
    	
    	//schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getGrade3().division(0).getClassroomSessionDivision(),null, Boolean.TRUE,Boolean.TRUE);
    }
        
}
