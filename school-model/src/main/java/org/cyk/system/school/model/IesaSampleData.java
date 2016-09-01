package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.RandomStringUtils;
import org.cyk.system.root.model.AbstractSampleData;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.utility.common.generator.RandomDataProvider;

public class IesaSampleData extends AbstractSampleData implements Serializable {

	private static final long serialVersionUID = -1887987316565799879L;
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForOtherGrade(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
				{"Formname(s)", report.getStudent().getPerson().getNames()}
				,{"Surname", report.getStudent().getPerson().getSurname()}
				/*,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
				,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
				,{"Admission No", report.getStudent().getRegistrationCode()}*/
				,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
				,{"Gender", report.getStudent().getPerson().getSex()}
				});
		
		report.addLabelValueCollection("SCHOOL ATTENDANCE",new String[][]{
				{"Number of times school opened","999"}
				,{"Number of times present","999"}
				,{"Number of times absent","999"}
				});
		
		report.addLabelValueCollection("OVERALL RESULT",new String[][]{
				{"AVERAGE","78.15"}
				,{"GRADE","A+"}
				,{"RANK","24"}
				});
		
		report.addLabelValueCollection("BEHAVIOUR,STUDY AND WORK HABITS",new String[][]{
				{"Respects authority", "4"}
				,{"Works independently and neatly", "2"}
				,{"Completes homework and class work on time", "3"}
				,{"Shows social courtesies", "4"}
				,{"Demonstrates self-control", "3"}
				,{"Takes care of school and others materials", "2"}
				});
		
		report.addLabelValueCollection("BEHAVIOUR,STUDY AND WORK HABITS",new String[][]{
				{"Game/Sport", "4"}
				,{"Handwriting", "3"}
				,{"Drawing/Painting", "4"}
				,{"Punctuality/Regularity", "4"}
				,{"Works cooperatively in groups", "2"}
				,{"Listens and follows directions", "2"}
				});
		

		report.addLabelValueCollection("GRADING SCALE",new String[][]{
				{"A+", "Excellent","90 - 100"}
				,{"A",  "Very Good","80 - 89.99"}
				,{"B+", "Good","70 - 79.99"}
				,{"B",  "Fair","60 - 69.99"}
				,{"C+", "Satisfactory","55 - 59.99"}
				,{"C",  "Barely satisfactory","50 - 54.99"}
				,{"E",  "Fail","00 - 49.99"}
				});
				
		report.addLabelValueCollection("EFFORT LEVELS",new String[][]{
				{"1", "Has no regard for the observable traits"}
				,{"2", "Shows minimal regard for the observable traits"}
				,{"3", "Acceptable level of observable traits"}
				,{"4", "Maintains high level of observable traits"}
				,{"5", "Maintains an excellent degree of observable traits"}
				});
		

		report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
				{"ANNUAL AVERAGE","90"}
				,{"ANNUAL GRADE","B+"}
				,{"ANNUAL RANK","25"}
				,{"PROMOTION INFORMATION","PROMOTED"}
				,{"NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015"}
				});
		
		report.addSubjectsTableColumnNames("No.","SUBJECTS","Test 1 15%","Test 2 15%","Exam 70%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForPreKinderGarten(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
				{"Formname(s)", report.getStudent().getPerson().getNames()}
				,{"Surname", report.getStudent().getPerson().getSurname()}
				/*,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
				,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
				,{"Admission No", report.getStudent().getRegistrationCode()}*/
				,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
				,{"Gender", report.getStudent().getPerson().getSex()}
				});
		
		report.addLabelValueCollection("ATTENDANCE RECORD",new String[][]{{"Days school opened","61"},{"Days Present","16"},{"Days Absent","45"}});

		report.addLabelValueCollection("EXPRESSIVE LANGUAGE",new String[][]{{"Participates actively during circle time","1"}
				,{"Participates in singing rhymes","NA"},{"Can say her name and name of classmates","1"},{"Can respond appropriately to “how are you?”","1"}
				,{"Can say his/her age","1"},{"Can say the name of her school","1"},{"Names objects in the classroom and school environment","1"}
				,{"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","1"},{"Talks in two or three word phrases and longer sentences","1"}
				,{"Can use “and” to connect words/phrases","1"},{"Talks with words in correct order","1"},{"Can be engaged in conversations","1"}});
		
		report.addLabelValueCollection("RECEPTIVE LANGUAGE",new String[][]{{"Responds to her name when called","1"}
				,{"Retrieves named objects","1"},{"Follows simple instructions (across the classroom) – stand, sit, bring your cup","1"}
				,{"Understands facial expressions and tone of voice","1"},{"Understands 2-3 step instructions","NA"}
				,{"Understands positional words – In and out - Up and down - On and under - Forward and backward","1"},{"Understands the concept “Give and Take”","1"}
				,{"Talks about feelings","1"}});
		
		report.addLabelValueCollection("READING READINESS",new String[][]{{"Shows interest in books/stories","1"}
				,{"Names familiar objects in pictures/books – vegetables, fruits, animals","1"},{"Tells what action is going on in pictures","1"}
				,{"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf","NA"},{"Listening for different sounds in the environment","NA"}
				,{"Identifying objects that begin with a particular sound","NA"},{"Identifying pictures that begin with a particular sound","NA"}
				,{"Recognizes the written letters of the alphabet","1"}});
		
		report.addLabelValueCollection("NUMERACY DEVELOPMENT",new String[][]{{"Sorts objects by shape","1"}
				,{"Sorts objects by size","1"},{"Participates in reciting different counting rhymes, songs, stories and games","1"}
				,{"Verbally count forward to 10","1"},{"Can count 1-10 objects","1"},{"Identifies the written numerals 1-10","1"}
				,{"Reproducing Patterns","NA"},{"Identifies the 3 basic geometric shapes ( circle,triangle and square)","1"}
				,{"Identifies more shapes ( Star, diamond, heart,cross ,crescent)","NA"}});
		
		report.addLabelValueCollection("ARTS AND MUSIC",new String[][]{{"Moves expressively to sounds and music – nodding, clapping, movement of body","2"},
				{"Participates in musical activities","1"},{"Hums or sing words of songs","1"},{"Participates in role play","1"},{"Shows satisfaction with completed work","1"}});
		
		report.addLabelValueCollection("SOCIAL AND EMOTIONAL DEVELOPMENT",new String[][]{{"Initiates interaction with adults","NA"},{"Initiates interaction with classmates","1"},
				{"Participates in group activities","1"},{"Takes turns during group activities","1"},{"Greets people – hello and goodbye","1"},{"Says “please” and “thank you”","1"},
				{"Asks for help in doing things when needed","1"},{"Shows sympathy, offers to help or helps others","1"},{"Can express dissatisfaction and other emotions – body language or words","2"},
				{"Responds to correction – stops the misbehaviour","2"}});
		
		report.addLabelValueCollection("GROSS MOTOR SKILLS",new String[][]{{"Can run well without falling","2"},
				{"Can kick a ball","1"},{"Climbs up ladder and slides down slide without help","2"},
				{"Walks up and down stairs unassisted","NA"},{"Can stand on one foot for a few seconds without support","NA"},{"Throws a ball into a basket from a short distance","1"}});
		
		report.addLabelValueCollection("FINE MOTOR SKILLS",new String[][]{{"Scribbles spontaneously","2"},{"Can scribble to and from, in circular motions and in lines","2"},
				{"Can place simple pieces in a puzzle board","1"},{"Can build a tower of at least 3-5 blocks","2"},{"Develops good pencil grip and control","1"}});
		
		report.addLabelValueCollection("SKILLS PERFORMANCE LEVELS",new String[][]{{"3","Does regularly"},{"2","Does sometimes"},{"1","Learning to do"},{"NA","Not Assessed"}});
		
		report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{{"Conference requested","NO"},{"Promotion in danger","YES"},
				{"School reopens","4th January 2016"},{"Next Term Examination","14th March 2016"}});
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForKG1(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
				{"Formname(s)", report.getStudent().getPerson().getNames()}
				,{"Surname", report.getStudent().getPerson().getSurname()}
				/*,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
				,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
				,{"Admission No", report.getStudent().getRegistrationCode()}*/
				,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
				,{"Gender", report.getStudent().getPerson().getSex()}
				});
		
		report.addLabelValueCollection("ATTENDANCE RECORD",new String[][]{{"Days school opened","61"},{"Days Present","16"},{"Days Absent","45"}});

		report.addLabelValueCollection("ENGLISH/LANGUAGE ARTS/READING",new String[][]{{"Participates actively during circle time","1"}
				,{"Participates in singing rhymes","NA"},{"Can say her name and name of classmates","1"},{"Can respond appropriately to “how are you?”","1"}
				,{"Can say his/her age","1"},{"Can say the name of her school","1"},{"Names objects in the classroom and school environment","1"}
				,{"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","1"},{"Talks in two or three word phrases and longer sentences","1"}
				,{"Can use “and” to connect words/phrases","1"},{"Talks with words in correct order","1"},{"Can be engaged in conversations","1"}});
		
		report.addLabelValueCollection("COMMUNICATION SKILLS",new String[][]{{"Responds to her name when called","1"}
				,{"Retrieves named objects","1"},{"Follows simple instructions (across the classroom) – stand, sit, bring your cup","1"}
				,{"Understands facial expressions and tone of voice","1"},{"Understands 2-3 step instructions","NA"}
				,{"Understands positional words – In and out - Up and down - On and under - Forward and backward","1"},{"Understands the concept “Give and Take”","1"}
				,{"Talks about feelings","1"}});
		
		report.addLabelValueCollection("SCIENCE",new String[][]{{"Shows interest in books/stories","1"}
				,{"Names familiar objects in pictures/books – vegetables, fruits, animals","1"},{"Tells what action is going on in pictures","1"}
				,{"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf","NA"},{"Listening for different sounds in the environment","NA"}
				,{"Identifying objects that begin with a particular sound","NA"},{"Identifying pictures that begin with a particular sound","NA"}
				,{"Recognizes the written letters of the alphabet","1"}});
		
		report.addLabelValueCollection("SOCIAL STUDIES",new String[][]{{"Sorts objects by shape","1"}
				,{"Sorts objects by size","1"},{"Participates in reciting different counting rhymes, songs, stories and games","1"}
				,{"Verbally count forward to 10","1"},{"Can count 1-10 objects","1"},{"Identifies the written numerals 1-10","1"}
				,{"Reproducing Patterns","NA"},{"Identifies the 3 basic geometric shapes ( circle,triangle and square)","1"}
				,{"Identifies more shapes ( Star, diamond, heart,cross ,crescent)","NA"}});
		
		report.addLabelValueCollection("MATHEMATICS",new String[][]{{"Moves expressively to sounds and music – nodding, clapping, movement of body","2"},
				{"Participates in musical activities","1"},{"Hums or sing words of songs","1"},{"Participates in role play","1"},{"Shows satisfaction with completed work","1"}});
		
		report.addLabelValueCollection("WORK HABITS",new String[][]{{"Initiates interaction with adults","NA"},{"Initiates interaction with classmates","1"},
				{"Participates in group activities","1"},{"Takes turns during group activities","1"},{"Greets people – hello and goodbye","1"},{"Says “please” and “thank you”","1"},
				{"Asks for help in doing things when needed","1"},{"Shows sympathy, offers to help or helps others","1"},{"Can express dissatisfaction and other emotions – body language or words","2"},
				{"Responds to correction – stops the misbehaviour","2"}});
		
		report.addLabelValueCollection("SOCIAL SKILLS",new String[][]{{"Can run well without falling","2"},
				{"Can kick a ball","1"},{"Climbs up ladder and slides down slide without help","2"},
				{"Walks up and down stairs unassisted","NA"},{"Can stand on one foot for a few seconds without support","NA"},{"Throws a ball into a basket from a short distance","1"}});
		
		report.addLabelValueCollection("SKILLS PERFORMANCE LEVELS",new String[][]{{"3","Does regularly"},{"2","Does sometimes"},{"1","Learning to do"},{"NA","Not Assessed"}});
		
		report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{{"Conference requested","NO"},{"Promotion in danger","YES"},
				{"School reopens","4th January 2016"},{"Next Term Examination","14th March 2016"}});
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForKG2KG3(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
				{"Formname(s)", report.getStudent().getPerson().getNames()}
				,{"Surname", report.getStudent().getPerson().getSurname()}
				/*,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
				,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
				,{"Admission No", report.getStudent().getRegistrationCode()}*/
				,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
				,{"Gender", report.getStudent().getPerson().getSex()}
				});
		
		report.addLabelValueCollection("ATTENDANCE RECORD",new String[][]{{"Days school opened","61"},{"Days Present","16"},{"Days Absent","45"}});

		report.addLabelValueCollection("READING READINESS",new String[][]{{"Demonstrates concepts of print","1"}
				,{"Identifies and produces rhyming words","NA"},{"Segments and blends sounds","2"}});
		
		report.addLabelValueCollection("READING",new String[][]{{"Answers questions about essential narrative elements","1"}
				,{"Reads high frequency words","1"},{"Blends sounds to read words","1"}
				,{"Reads simple text","1"},{"Developmental Reading assessment","NA"}});
		
		report.addLabelValueCollection("WRITING",new String[][]{{"Writes first and last name","1"}
				,{"Expresses ideas through independent writing","1"}});
		
		report.addLabelValueCollection("LISTENING, SPEAKING AND VIEWING",new String[][]{{"Uses oral language to communicate effectively","1"}
				,{"Recites short poems and songs","1"},{"Follows two-step oral directions","1"}
				,{"Makes predictions and retells","1"},{"Comprehends information through listening","1"},{"Demonstrates comprehension of information through speaking","1"}
				});
		
		report.addLabelValueCollection("ALPHABET IDENTIFICATION",new String[][]{{"Identifies Upper-Case","2"},
				{"Identifies Lower-Case","1"},{"Produces Letter Sounds","1"},{"Prints Letters Correctly","1"}});
		
		report.addLabelValueCollection("MATHEMATICS",new String[][]{{"Number and Operations","NA"},{"Geometry","1"},{"Measurement","1"},{"Algebraic Thinking","1"}});
		
		report.addLabelValueCollection("SCIENCE, SOCIAL STUDIES AND MORAL EDUCATION",new String[][]{{"Science","2"},{"Social Studies","1"},{"Moral Education","2"}});
		
		report.addLabelValueCollection("ART and CRAFT",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValueCollection("MUSIC",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValueCollection("PHYSICAL EDUCATION",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValueCollection("WORK and BEHAVIOUR HABITS",new String[][]{{"Follows directions","2"},{"Uses time and materials constructively","1"},{"Works independently","NA"}
		,{"Completes class assignments","1"},{"Completes homework assignments","2"},{"Listens attentively","3"},{"Cooperates with others","NA"},{"Shows respect for others","3"}
		,{"Participates in classroom activities","2"},{"Follows classroom/school rules","5"}});
		
		report.addLabelValueCollection("SKILLS PERFORMANCE LEVELS",new String[][]{{"3","Does regularly"},{"2","Does sometimes"},{"1","Learning to do"},{"NA","Not Assessed"}});
		
		report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{{"Conference requested","NO"},{"Promotion in danger","YES"},
				{"School reopens","4th January 2016"},{"Next Term Examination","14th March 2016"}});
		
		return collection;
	}
		
	public static void main(String[] args) {
		Collection<StudentClassroomSessionDivisionReport> reports = createStudentClassroomSessionDivisionReportsForOtherGrade();
		System.out.println("IesaSampleData.main() : "+reports.iterator().next().getClassroomSessionDivisionSubjects().size());
	}
	
}
