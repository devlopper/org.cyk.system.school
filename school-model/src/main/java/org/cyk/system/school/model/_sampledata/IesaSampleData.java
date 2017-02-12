package org.cyk.system.school.model._sampledata;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.RandomStringUtils;
import org.cyk.system.root.model.AbstractSampleData;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.utility.common.generator.RandomDataProvider;

public class IesaSampleData extends AbstractSampleData implements Serializable {

	private static final long serialVersionUID = -1887987316565799879L;
	
	protected static Collection<StudentClassroomSessionDivisionReportTemplateFile> __createStudentClassroomSessionDivisionReportsForOtherGrade__(Long classroomSessionDivisionOrderNumber,Boolean provisional){
		Collection<StudentClassroomSessionDivisionReportTemplateFile> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReportTemplateFile.class, 1);
		StudentClassroomSessionDivisionReportTemplateFile report = collection.iterator().next();
		report.setIsDraft(provisional);
		report.setName(classroomSessionDivisionOrderNumber+" TERM - G1-G12 REPORT SHEET");
		report.getStudentClassroomSessionDivision().getClassroomSessionDivision().setAverage("12");
		if(Boolean.TRUE.equals(report.getIsDraft())){
			report.getStudentClassroomSessionDivision().getAcademicSession().getCompany().setDraftBackground(Boolean.TRUE);
			report.getStudentClassroomSessionDivision().getAcademicSession().getCompany().generate();
			report.setBackgroundImage(report.getStudentClassroomSessionDivision().getAcademicSession().getCompany().getBackgroundImage());
			report.setFooter("This Provisional Results Information is not an official document and is for information only.");
		}
		addPupilsDetails(report);
		addSchoolAttendance(report);
		addOverallResult(report);
		addBehaviour1(report);
		addBehaviour2(report);
		addGradingScale(report);
		addEffortLevels(report);
		addSchoolCommunications(report,Boolean.FALSE);
		addSubjectsTableColumnNames(report);
		addPreviousOverallResult(classroomSessionDivisionOrderNumber,report);
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createFirstTermStudentClassroomSessionDivisionReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(1l,Boolean.FALSE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createSecondTermStudentClassroomSessionDivisionReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(2l,Boolean.FALSE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createThirdTermStudentClassroomSessionDivisionReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(3l,Boolean.FALSE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createFirstStudentClassroomSessionDivisionProvisionalReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(1l,Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createSecondStudentClassroomSessionDivisionProvisionalReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(2l,Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createThirdStudentClassroomSessionDivisionProvisionalReportsForOtherGrade(){
		return __createStudentClassroomSessionDivisionReportsForOtherGrade__(3l,Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionReportsForPreKinderGarten(){
		return __createStudentClassroomSessionDivisionReportsForPreKinderGarten__(Boolean.FALSE);
	}
		
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionProvisionalReportsForPreKinderGarten(){
		return __createStudentClassroomSessionDivisionReportsForPreKinderGarten__(Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> __createStudentClassroomSessionDivisionReportsForPreKinderGarten__(Boolean provisional){
		Collection<StudentClassroomSessionDivisionReportTemplateFile> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReportTemplateFile.class, 1);
		StudentClassroomSessionDivisionReportTemplateFile report = collection.iterator().next();
		report.setIsDraft(provisional);
		report.setName("PRE-KINDERGARTEN REPORT SHEET");
		report.getStudentClassroomSessionDivision().setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.getStudentClassroomSessionDivision().setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		addPupilsDetails(report);
		addSchoolAttendance(report);
		
		report.addLabelValues("EXPRESSIVE LANGUAGE",new String[][]{{"Participates actively during circle time","1"}
				,{"Participates in singing rhymes","NA"},{"Can say her name and name of classmates","1"},{"Can respond appropriately to “how are you?”","1"}
				,{"Can say his/her age","1"},{"Can say the name of her school","1"},{"Names objects in the classroom and school environment","1"}
				,{"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","1"},{"Talks in two or three word phrases and longer sentences","1"}
				,{"Can use “and” to connect words/phrases","1"},{"Talks with words in correct order","1"},{"Can be engaged in conversations","1"}});
		
		report.addLabelValues("RECEPTIVE LANGUAGE",new String[][]{{"Responds to her name when called","1"}
				,{"Retrieves named objects","1"},{"Follows simple instructions (across the classroom) – stand, sit, bring your cup","1"}
				,{"Understands facial expressions and tone of voice","1"},{"Understands 2-3 step instructions","NA"}
				,{"Understands positional words – In and out - Up and down - On and under - Forward and backward","1"},{"Understands the concept “Give and Take”","1"}
				,{"Talks about feelings","1"}});
		
		report.addLabelValues("READING READINESS",new String[][]{{"Shows interest in books/stories","1"}
				,{"Names familiar objects in pictures/books – vegetables, fruits, animals","1"},{"Tells what action is going on in pictures","1"}
				,{"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf","NA"},{"Listening for different sounds in the environment","NA"}
				,{"Identifying objects that begin with a particular sound","NA"},{"Identifying pictures that begin with a particular sound","NA"}
				,{"Recognizes the written letters of the alphabet","1"}});
		
		report.addLabelValues("NUMERACY DEVELOPMENT",new String[][]{{"Sorts objects by shape","1"}
				,{"Sorts objects by size","1"},{"Participates in reciting different counting rhymes, songs, stories and games","1"}
				,{"Verbally count forward to 10","1"},{"Can count 1-10 objects","1"},{"Identifies the written numerals 1-10","1"}
				,{"Reproducing Patterns","NA"},{"Identifies the 3 basic geometric shapes ( circle,triangle and square)","1"}
				,{"Identifies more shapes ( Star, diamond, heart,cross ,crescent)","NA"}});
		
		report.addLabelValues("ARTS AND MUSIC",new String[][]{{"Moves expressively to sounds and music – nodding, clapping, movement of body","2"},
				{"Participates in musical activities","1"},{"Hums or sing words of songs","1"},{"Participates in role play","1"},{"Shows satisfaction with completed work","1"}});
		
		report.addLabelValues("SOCIAL AND EMOTIONAL DEVELOPMENT",new String[][]{{"Initiates interaction with adults","NA"},{"Initiates interaction with classmates","1"},
				{"Participates in group activities","1"},{"Takes turns during group activities","1"},{"Greets people – hello and goodbye","1"},{"Says “please” and “thank you”","1"},
				{"Asks for help in doing things when needed","1"},{"Shows sympathy, offers to help or helps others","1"},{"Can express dissatisfaction and other emotions – body language or words","2"},
				{"Responds to correction – stops the misbehaviour","2"}});
		
		report.addLabelValues("GROSS MOTOR SKILLS",new String[][]{{"Can run well without falling","2"},
				{"Can kick a ball","1"},{"Climbs up ladder and slides down slide without help","2"},
				{"Walks up and down stairs unassisted","NA"},{"Can stand on one foot for a few seconds without support","NA"},{"Throws a ball into a basket from a short distance","1"}});
		
		report.addLabelValues("FINE MOTOR SKILLS",new String[][]{{"Scribbles spontaneously","2"},{"Can scribble to and from, in circular motions and in lines","2"},
				{"Can place simple pieces in a puzzle board","1"},{"Can build a tower of at least 3-5 blocks","2"},{"Develops good pencil grip and control","1"}});
		
		addSkillPerformanceLevels(report);
		addSchoolCommunications(report,Boolean.TRUE);
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionReportsForKG1(){
		return __createStudentClassroomSessionDivisionReportsForKG1__(Boolean.FALSE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionProvisionalReportsForKG1(){
		return __createStudentClassroomSessionDivisionReportsForKG1__(Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> __createStudentClassroomSessionDivisionReportsForKG1__(Boolean provisional){
		Collection<StudentClassroomSessionDivisionReportTemplateFile> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReportTemplateFile.class, 1);
		StudentClassroomSessionDivisionReportTemplateFile report = collection.iterator().next();
		report.setIsDraft(provisional);
		report.setName("KINDERGARTEN REPORT SHEET");
		report.getStudentClassroomSessionDivision().setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.getStudentClassroomSessionDivision().setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		addPupilsDetails(report);
		addSchoolAttendance(report);

		report.addLabelValues("ENGLISH/LANGUAGE ARTS/READING",new String[][]{{"Reads independently with understanding","1"}
				,{"Comprehends a variety of texts","NA"},{"Applies a variety of strategies to comprehendcprinted text","1"}
				,{"Reads to access and utilize information from written and electronic sources","1"}
				,{"Demonstrates understanding of letter-sound associations","1"}});
		
		report.addLabelValues("COMMUNICATION SKILLS",new String[][]{{"Contributes ideas to discussions","1"}
				,{"Communicates ideas effectively","1"},{"Write for a variety of purposes","1"}
				,{"Writes well-organized compositions","1"},{"Uses appropriate writing skills","NA"}
				,{"Write legibly","1"},{"Revises, edits and proofreads work","1"}});
		
		report.addLabelValues("SCIENCE",new String[][]{{"Understands and applies scientific process","1"}
			,{"Understands and applies knowledge of key concepts","1"}});
		
		report.addLabelValues("SOCIAL STUDIES",new String[][]{{"Gathers and organizes information","1"}
				,{"Understands and applies knowledge of key concepts","1"}});
				
		report.addLabelValues("MATHEMATICS",new String[][]{{"Demonstrates understanding of number sense","2"},
				{"Reads and interprets data","1"},{"Applies problem-solving strategies","1"},{"Communicates mathematically","1"}});
		
		report.addLabelValues("WORK HABITS",new String[][]{{"Follows directions","NA"},{"Uses time and materials constructively","1"},
				{"Works independently","1"},{"Completes class assignments","1"},{"Completes homework assignments","1"},{"Listens attentively","1"}});
		
		report.addLabelValues("SOCIAL SKILLS",new String[][]{{"Cooperates with others","2"},
				{"Shows respect for others","1"},{"Participates in classroom activities","2"},
				{"Follows classroom/school rules","NA"}});
		
		addSkillPerformanceLevels(report);
		addContentMarkingCodes(report);
		addSchoolCommunications(report,Boolean.TRUE);
		
		return collection;
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionReportsForKG2KG3(){
		return __createStudentClassroomSessionDivisionReportsForKG2KG3__(Boolean.FALSE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> createStudentClassroomSessionDivisionProvisionalReportsForKG2KG3(){
		return __createStudentClassroomSessionDivisionReportsForKG2KG3__(Boolean.TRUE);
	}
	
	public static Collection<StudentClassroomSessionDivisionReportTemplateFile> __createStudentClassroomSessionDivisionReportsForKG2KG3__(Boolean provisional){
		Collection<StudentClassroomSessionDivisionReportTemplateFile> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReportTemplateFile.class, 1);
		StudentClassroomSessionDivisionReportTemplateFile report = collection.iterator().next();
		report.setIsDraft(provisional);
		report.setName("KINDERGARTEN REPORT SHEET");
		report.getStudentClassroomSessionDivision().setSchoolStampBlockTitle("SCHOOL STAMP AND SIGNATURE");
		report.getStudentClassroomSessionDivision().setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
				
		addPupilsDetails(report);
		addSchoolAttendance(report);

		report.addLabelValues("READING READINESS",new String[][]{{"Demonstrates concepts of print","1"}
				,{"Identifies and produces rhyming words","NA"},{"Segments and blends sounds","2"}});
		
		report.addLabelValues("READING",new String[][]{{"Answers questions about essential narrative elements","1"}
				,{"Reads high frequency words","1"},{"Blends sounds to read words","1"}
				,{"Reads simple text","1"},{"Developmental Reading assessment","NA"}});
		
		report.addLabelValues("WRITING",new String[][]{{"Writes first and last name","1"}
				,{"Expresses ideas through independent writing","1"}});
		
		report.addLabelValues("LISTENING, SPEAKING AND VIEWING",new String[][]{{"Uses oral language to communicate effectively","1"}
				,{"Recites short poems and songs","1"},{"Follows two-step oral directions","1"}
				,{"Makes predictions and retells","1"},{"Comprehends information through listening","1"},{"Demonstrates comprehension of information through speaking","1"}
				});
		
		report.addLabelValues("ALPHABET IDENTIFICATION",new String[][]{{"Identifies Upper-Case","2"},
				{"Identifies Lower-Case","1"},{"Produces Letter Sounds","1"},{"Prints Letters Correctly","1"}});
		
		report.addLabelValues("MATHEMATICS",new String[][]{{"Number and Operations","NA"},{"Geometry","1"},{"Measurement","1"},{"Algebraic Thinking","1"}});
		
		report.addLabelValues("SCIENCE, SOCIAL STUDIES AND MORAL EDUCATION",new String[][]{{"Science","2"},{"Social Studies","1"},{"Moral Education","2"}});
		
		report.addLabelValues("ART and CRAFT",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValues("MUSIC",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValues("PHYSICAL EDUCATION",new String[][]{{"Performance","2"},{"Initiative","1"}});
		
		report.addLabelValues("WORK and BEHAVIOUR HABITS",new String[][]{{"Follows directions","2"},{"Uses time and materials constructively","1"},{"Works independently","NA"}
		,{"Completes class assignments","1"},{"Completes homework assignments","2"},{"Listens attentively","3"},{"Cooperates with others","NA"},{"Shows respect for others","3"}
		,{"Participates in classroom activities","2"},{"Follows classroom/school rules","5"}});
		
		addPerformanceCodes(report);
		addSchoolCommunications(report,Boolean.TRUE);
		
		return collection;
	}
	
	private static void addSubjectsTableColumnNames(StudentClassroomSessionDivisionReportTemplateFile report){
		report.getStudentClassroomSessionDivision().addSubjectsTableColumnNames("No.","SUBJECTS","Test 1 15%","Test 2 15%","Exam 70%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
	}
	
	private static void addPupilsDetails(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("PUPIL'S DETAILS",new String[][]{
			{"Formname(s)", report.getStudentClassroomSessionDivision().getStudent().getPerson().getLastnames()}
			,{"Surname", report.getStudentClassroomSessionDivision().getStudent().getPerson().getGlobalIdentifier().getName()}
			,{"Date of birth", report.getStudentClassroomSessionDivision().getStudent().getPerson().getGlobalIdentifier().getExistencePeriod().getFrom()}
			,{"Place of birth", report.getStudentClassroomSessionDivision().getStudent().getPerson().getGlobalIdentifier().getBirthLocation()}
			,{"Admission No", report.getStudentClassroomSessionDivision().getStudent().getGlobalIdentifier().getCode()}
			,{"Class", report.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession().getName()}
			,{"Gender", report.getStudentClassroomSessionDivision().getStudent().getPerson().getSex()}
			});
	}
	
	private static void addSchoolAttendance(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("SCHOOL ATTENDANCE",new String[][]{
			{"Number of times school opened","999"}
			,{"Number of times present","999"}
			,{"Number of times absent","999"}
			,{"Number of time suspended","999"}
			,{"Number of time on detention","999"}
			});
	}
	
	private static void addOverallResult(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("OVERALL RESULT",new String[][]{
			{"AVERAGE","78.15"}
			,{"GRADE","A+"}
			,{"RANK","24th"}
			});
	}
	
	private static void addPreviousOverallResult(Long classroomSessionDivisionOrderNumber,StudentClassroomSessionDivisionReportTemplateFile report){
		if(classroomSessionDivisionOrderNumber>1)
			report.addLabelValues("PREVIOUS RESULTS",new String[][]{	
				{"AVERAGE","42",classroomSessionDivisionOrderNumber > 2 ? "55" : null}
				,{"GRADE","E",classroomSessionDivisionOrderNumber > 2 ? "B" : null}
				,{"RANK","3th",classroomSessionDivisionOrderNumber > 2 ? "4th" : null}
				,{"TRIMESTER","1",classroomSessionDivisionOrderNumber > 2 ? "2" : null}
				});
	}
	
	private static void addGradingScale(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("GRADING SCALE",new String[][]{
			{"A+", "Excellent","90 - 100"}
			,{"A",  "Very Good","80 - 89.99"}
			,{"B+", "Good","70 - 79.99"}
			,{"B",  "Fair","60 - 69.99"}
			,{"C+", "Satisfactory","55 - 59.99"}
			,{"C",  "Barely satisfactory","50 - 54.99"}
			,{"E",  "Fail","00 - 49.99"}
			});
	}
	
	private static void addEffortLevels(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("EFFORT LEVELS",new String[][]{
			{"1", "Has no regard for the observable traits"}
			,{"2", "Shows minimal regard for the observable traits"}
			,{"3", "Acceptable level of observable traits"}
			,{"4", "Maintains high level of observable traits"}
			,{"5", "Maintains an excellent degree of observable traits"}
			});
	}
	
	private static void addSchoolCommunications(StudentClassroomSessionDivisionReportTemplateFile report,Boolean kindergarten){
		report.addLabelValues("HOME/SCHOOL COMMUNICATIONS",new String[][]{
			{Boolean.TRUE.equals(kindergarten) ? null : "ANNUAL AVERAGE",Boolean.TRUE.equals(kindergarten) ? null : "90"}
			,{Boolean.TRUE.equals(kindergarten) ? null : "ANNUAL GRADE",Boolean.TRUE.equals(kindergarten) ? null : "B+"}
			,{Boolean.TRUE.equals(kindergarten) ? null : "ANNUAL RANK",Boolean.TRUE.equals(kindergarten) ? null : "25"}
			,{"PROMOTION INFORMATION","PROMOTED"}
			,{"NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015"}
			});
	}
	
	private static void addSkillPerformanceLevels(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("SKILLS PERFORMANCE LEVELS",new String[][]{{"3","Does regularly"},{"2","Does sometimes"},{"1","Learning to do"},{"NA","Not Assessed"}});
	}
	
	private static void addContentMarkingCodes(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("CONTENT MARKING CODES",new String[][]{{"A","94 - 100"},{"B","85 - 93"},{"C","77 - 84"}
		,{"D","70 - 76"},{"F","0 - 75"},{"NA","Not Assessed"}});
	}
	
	private static void addPerformanceCodes(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("PERFORMANCE CODES",new String[][]{{"4","Meets and applies expectations/standards independently"}
		,{"3","Meets and applies expectations/standards with support"},{"2","Does not meets and applies expectations/standards; but shows growth with support"}
		,{"1","Does not meets and applies expectations/standards; shows no growth even with support"},{"NA","Not Assessed"}});
	}
	
	private static void addBehaviour1(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("BEHAVIOUR,STUDY AND WORK HABITS",new String[][]{
			{"Respects authority", "4"}
			,{"Works independently and neatly", "2"}
			,{"Completes homework and class work on time", "3"}
			,{"Shows social courtesies", "4"}
			,{"Demonstrates self-control", "3"}
			,{"Takes care of school and others materials", "2"}
			,{"Event management skills", "1"}
			});
	}
	
	private static void addBehaviour2(StudentClassroomSessionDivisionReportTemplateFile report){
		report.addLabelValues("BEHAVIOUR,STUDY AND WORK HABITS",new String[][]{
			{"Game/Sport", "4"}
			,{"Handwriting", "3"}
			,{"Drawing/Painting", "4"}
			,{"Punctuality/Regularity", "4"}
			,{"Works cooperatively in groups", "2"}
			,{"Listens and follows directions", "2"}
			,{"Community", "2"}
			});
	}
	
	public static void main(String[] args) {
		Collection<StudentClassroomSessionDivisionReportTemplateFile> reports = createFirstTermStudentClassroomSessionDivisionReportsForOtherGrade();
		System.out.println("IesaSampleData.main() : "+reports.iterator().next().getStudentClassroomSessionDivision().getClassroomSessionDivisionSubjects().size());
	}
	
}
