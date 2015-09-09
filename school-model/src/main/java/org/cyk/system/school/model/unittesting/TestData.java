package org.cyk.system.school.model.unittesting;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.school.model.session.StudentClassroomSessionDivisionResultReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionResultReportSubjectDetail;
import org.cyk.utility.common.generator.RandomDataProvider;

public class TestData {

	public static Collection<StudentClassroomSessionDivisionResultReport> studentClassroomSessionDivisionResultReports(){
		Collection<StudentClassroomSessionDivisionResultReport> collection = new ArrayList<>();
		collection.add(generate("1"));
		collection.add(generate("2"));
		collection.add(generate("3"));
		collection.add(generate("4"));
		collection.add(generate("5"));
		collection.add(generate("6"));
		return collection;
	}
	
	private static StudentClassroomSessionDivisionResultReport generate(String orderNumber){
		StudentClassroomSessionDivisionResultReport report = new StudentClassroomSessionDivisionResultReport();
			
		report.setAcademicSession("2000-2001");
		report.setAppreciation("Encouragements");
		report.setAppreciationCommentedBy("Koffi jean");
		report.setAppreciationComments("Bon rendements");
		report.setAverage("11");
		report.setClassroomSession("6eme 3");
		report.setClassroomSessionAverage("11.3");
		report.setClassroomSessionAverageHighest("11.5");
		report.setClassroomSessionAverageLowest("9.76");
		report.setDateOfBirth("10/03/1987");
		report.setFooter(RandomDataProvider.getInstance().randomLine(3, 5));
		
		report.setGovernmentMinistryInfos("Republique de COTE D'IVOIRE<br/>"
				+ "_____<br/>"
				+ "Union - Discipline - Travail<br/>"
				+ "_______<br/>"
				+ "Ministere de l'education nationale");
		
		report.setNames("Yao jules");
		report.setNumberOfStudents("25");
		report.setOrderNumber(orderNumber);
		report.setPhoto(new ByteArrayInputStream(RandomDataProvider.getInstance().getMale().photo()));
		report.setRank("12");
		 
		report.setSchoolName(RandomDataProvider.getInstance().randomLine(1, 2));
		report.setSchoolLogo(new ByteArrayInputStream(RandomDataProvider.getInstance().companyLogo()));
		
		report.setSignatureInfos("Abidjan le 10/10/10");
		report.setStaffPerson("Zoua Fulbert");
		report.setStaffTitle("La Direction"); 
		report.setTitle("Bulletin de note de 2eme Trimestre");
		
		report.setTotalAverage("10");
		report.setTotalAverageCoefficiented("10");
		report.setTotalMissedHours("5");
		report.setTotalMissedHoursJustified("2");
		
		report.getSubjects().add(new StudentClassroomSessionDivisionResultReportSubjectDetail(report,"Anglais","7.66","3","22.45","15","Pas bon","Zebie yves"));
		report.getSubjects().add(new StudentClassroomSessionDivisionResultReportSubjectDetail(report,"Histoire","12","1","12","4","Bien","Allou kola"));
		report.getSubjects().add(new StudentClassroomSessionDivisionResultReportSubjectDetail(report,"Physiques","8","4","32","18","Faible","Wale kangah"));
		
		return report;
	}
	
	public static void main(String[] args) {
		System.out.println("TestData.main()");
	}
}
