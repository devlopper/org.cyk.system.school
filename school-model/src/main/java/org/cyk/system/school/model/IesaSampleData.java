package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.RandomStringUtils;
import org.cyk.system.root.model.AbstractSampleData;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.utility.common.generator.RandomDataProvider;

public class IesaSampleData extends AbstractSampleData implements Serializable {

	private static final long serialVersionUID = -1887987316565799879L;
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForOtherGrade(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		LabelValueCollectionReport labelValueCollectionReport;
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("PUPIL'S DETAILS");
		labelValueCollectionReport.add("Formname(s)", report.getStudent().getPerson().getNames());
		labelValueCollectionReport.add("Surname", report.getStudent().getPerson().getSurname());
		labelValueCollectionReport.add("Date of birth", report.getStudent().getPerson().getBirthDate());
		labelValueCollectionReport.add("Place of birth", report.getStudent().getPerson().getBirthLocation());
		labelValueCollectionReport.add("Admission No", report.getStudent().getRegistrationCode());
		labelValueCollectionReport.add("Class", report.getClassroomSessionDivision().getClassroomSession().getName());
		labelValueCollectionReport.add("Gender", report.getStudent().getPerson().getSex());
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("SCHOOL ATTENDANCE");
		labelValueCollectionReport.add("Number of times school opened","999");
		labelValueCollectionReport.add("Number of times present","999");
		labelValueCollectionReport.add("Number of times absent","999");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("OVERALL RESULT");
		labelValueCollectionReport.add("AVERAGE","78.15");
		labelValueCollectionReport.add("GRADE","A+");
		labelValueCollectionReport.add("RANK","24");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("BEHAVIOUR,STUDY AND WORK HABITS");
		labelValueCollectionReport.add("Respects authority", "4");
		labelValueCollectionReport.add("Works independently and neatly", "2");
		labelValueCollectionReport.add("Completes homework and class work on time", "3");
		labelValueCollectionReport.add("Shows social courtesies", "4");
		labelValueCollectionReport.add("Demonstrates self-control", "3");
		labelValueCollectionReport.add("Takes care of school and others materials", "2");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.add("Game/Sport", "4");
		labelValueCollectionReport.add("Handwriting", "3");
		labelValueCollectionReport.add("Drawing/Painting", "4");
		labelValueCollectionReport.add("Punctuality/Regularity", "4");
		labelValueCollectionReport.add("Works cooperatively in groups", "2");
		labelValueCollectionReport.add("Listens and follows directions", "2");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("GRADING SCALE");
		LabelValueReport labelValueReport = labelValueCollectionReport.add("A+", "Excellent");
		labelValueReport.addExtendedValues("90 - 100");
		
		labelValueReport = labelValueCollectionReport.add("A",  "Very Good");
		labelValueReport.addExtendedValues("80 - 89.99");
		
		labelValueReport = labelValueCollectionReport.add("B+", "Good");
		labelValueReport.addExtendedValues("70 - 79.99");
		
		labelValueReport = labelValueCollectionReport.add("B",  "Fair");
		labelValueReport.addExtendedValues("60 - 69.99");
		
		labelValueReport = labelValueCollectionReport.add("C+", "Satisfactory");
		labelValueReport.addExtendedValues("55 - 59.99");
		
		labelValueReport = labelValueCollectionReport.add("C",  "Barely satisfactory");
		labelValueReport.addExtendedValues("50 - 54.99");
		
		labelValueReport = labelValueCollectionReport.add("E",  "Fail");
		labelValueReport.addExtendedValues("00 - 49.99");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("EFFORT LEVELS");
		labelValueCollectionReport.add("1", "Has no regard for the observable traits");
		labelValueCollectionReport.add("2", "Shows minimal regard for the observable traits");
		labelValueCollectionReport.add("3", "Acceptable level of observable traits");
		labelValueCollectionReport.add("4", "Maintains high level of observable traits");
		labelValueCollectionReport.add("5", "Maintains an excellent degree of observable traits");
		
		report.getLabelValueCollections().add(labelValueCollectionReport = new LabelValueCollectionReport());
		labelValueCollectionReport.setName("HOME/SCHOOL COMMUNICATIONS");
		labelValueCollectionReport.add("ANNUAL AVERAGE","90");
		labelValueCollectionReport.add("ANNUAL GRADE","B+");
		labelValueCollectionReport.add("ANNUAL RANK","25");
		labelValueCollectionReport.add("PROMOTION INFORMATION","PROMOTED");
		labelValueCollectionReport.add("NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015");
		
		report.getSubjectsTableColumnNames().add("No.");
		report.getSubjectsTableColumnNames().add("SUBJECTS");
		report.getSubjectsTableColumnNames().add("Test 1 15%");
		report.getSubjectsTableColumnNames().add("Test 2 15%");
		report.getSubjectsTableColumnNames().add("Exam 70%");
		report.getSubjectsTableColumnNames().add("TOTAL");
		report.getSubjectsTableColumnNames().add("GRADE");
		report.getSubjectsTableColumnNames().add("RANK");
		report.getSubjectsTableColumnNames().add("OUT OF");
		report.getSubjectsTableColumnNames().add("MAX");
		report.getSubjectsTableColumnNames().add("CLASS AVERAGE");
		report.getSubjectsTableColumnNames().add("REMARKS");
		report.getSubjectsTableColumnNames().add("TEACHER");
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForKinderGarten(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		setLabelValueCollection(report,"PUPIL'S DETAILS",new String[]{
				"Formname(s)", report.getStudent().getPerson().getNames()
				,"Surname", report.getStudent().getPerson().getSurname()
				,"Date of birth", report.getStudent().getPerson().getBirthDate()
				,"Place of birth", report.getStudent().getPerson().getBirthLocation()
				,"Admission No", report.getStudent().getRegistrationCode()
				,"Class", report.getClassroomSessionDivision().getClassroomSession().getName()
				,"Gender", report.getStudent().getPerson().getSex()
				});
		
		setLabelValueCollection(report,"ATTENDANCE RECORD",new String[]{"Days school opened","61","Days Present","16","Days Absent","45"});

		setLabelValueCollection(report,"EXPRESSIVE LANGUAGE",new String[]{"Participates actively during circle time","1"
				,"Participates in singing rhymes","NA","Can say her name and name of classmates","1","Can respond appropriately to “how are you?”","1"
				,"Can say his/her age","1","Can say the name of her school","1","Names objects in the classroom and school environment","1"
				,"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","1","Talks in two or three word phrases and longer sentences","1"
				,"Can use “and” to connect words/phrases","1","Talks with words in correct order","1","Can be engaged in conversations","1"});
		
		setLabelValueCollection(report,"RECEPTIVE LANGUAGE",new String[]{"Responds to her name when called","1"
				,"Retrieves named objects","1","Follows simple instructions (across the classroom) – stand, sit, bring your cup","1"
				,"Understands facial expressions and tone of voice","1","Understands 2-3 step instructions","NA"
				,"Understands positional words – In and out - Up and down - On and under - Forward and backward","1","Understands the concept “Give and Take”","1"
				,"Talks about feelings","1"});
		
		setLabelValueCollection(report,"READING READINESS",new String[]{"Shows interest in books/stories","1"
				,"Names familiar objects in pictures/books – vegetables, fruits, animals","1","Tells what action is going on in pictures","1"
				,"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf","NA","Listening for different sounds in the environment","NA"
				,"Identifying objects that begin with a particular sound","NA","Identifying pictures that begin with a particular sound","NA"
				,"Recognizes the written letters of the alphabet","1"});
		
		setLabelValueCollection(report,"NUMERACY DEVELOPMENT",new String[]{"Sorts objects by shape","1"
				,"Sorts objects by size","1","Participates in reciting different counting rhymes, songs, stories and games","1"
				,"Verbally count forward to 10","1","Can count 1-10 objects","1","Identifies the written numerals 1-10","1"
				,"Reproducing Patterns","NA","Identifies the 3 basic geometric shapes ( circle,triangle and square)","1"
				,"Identifies more shapes ( Star, diamond, heart,cross ,crescent)","NA"});
		
		setLabelValueCollection(report,"ARTS AND MUSIC",new String[]{"Moves expressively to sounds and music – nodding, clapping, movement of body","2",
				"Participates in musical activities","1","Hums or sing words of songs","1","Participates in role play","1","Shows satisfaction with completed work","1"});
		
		setLabelValueCollection(report,"SOCIAL AND EMOTIONAL DEVELOPMENT",new String[]{"Initiates interaction with adults","NA","Initiates interaction with classmates","1",
				"Participates in group activities","1","Takes turns during group activities","1","Greets people – hello and goodbye","1","Says “please” and “thank you”","1",
				"Asks for help in doing things when needed","1","Shows sympathy, offers to help or helps others","1","Can express dissatisfaction and other emotions – body language or words","2",
				"Responds to correction – stops the misbehaviour","2"});
		
		setLabelValueCollection(report,"GROSS MOTOR SKILLS",new String[]{"Can run well without falling","2",
				"Can kick a ball","1","Climbs up ladder and slides down slide without help","2",
				"Walks up and down stairs unassisted","NA","Can stand on one foot for a few seconds without support","NA","Throws a ball into a basket from a short distance","1"});
		
		setLabelValueCollection(report,"FINE MOTOR SKILLS",new String[]{"Scribbles spontaneously","2","Can scribble to and from, in circular motions and in lines","2",
				"Can place simple pieces in a puzzle board","1","Can build a tower of at least 3-5 blocks","2","Develops good pencil grip and control","1"});
		
		setLabelValueCollection(report,"SKILLS PERFORMANCE LEVELS",new String[]{"3","Does regularly","2","Does sometimes","1","Learning to do","NA","Not Assessed"});
		
		setLabelValueCollection(report,"HOME/SCHOOL COMMUNICATIONS",new String[]{"Conference requested","NO","Promotion in danger","YES",
				"School reopens","4th January 2016","Next Term Examination","14th March 2016"});
		
		return collection;
	}
		
	public static void main(String[] args) {
		Collection<StudentClassroomSessionDivisionReport> reports = createStudentClassroomSessionDivisionReportsForOtherGrade();
		System.out.println("IesaSampleData.main() : "+reports.iterator().next().getClassroomSessionDivisionSubjects().size());
	}
	
}
