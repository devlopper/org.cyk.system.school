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
		return generate(StudentClassroomSessionDivisionReport.class, 1);
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReportsForKinderGarten(){
		Collection<StudentClassroomSessionDivisionReport> collection = RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
		StudentClassroomSessionDivisionReport report = collection.iterator().next();
		
		report.setComments(RandomStringUtils.randomAlphabetic(300).toUpperCase()+"_END");
		report.getLabelValueCollections().clear();

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
		
		setLabelValueCollection(report,"ATTENDANCE RECORD",new String[]{"Days school opened","61","Days Present","16","Days Absent","45"});
		
		setLabelValueCollection(report,"HOME/SCHOOL COMMUNICATIONS",new String[]{"Conference requested","NO","Promotion in danger","YES",
				"School reopens","4th January 2016","Next Term Examination","14th March 2016"});
		
		setLabelValueCollection(report,"SKILLS PERFORMANCE LEVELS",new String[]{"3","Does regularly","2","Does sometimes","1","Learning to do","NA","Not Assessed"});
		return collection;
	}
		
	public static void main(String[] args) {
		Collection<StudentClassroomSessionDivisionReport> reports = createStudentClassroomSessionDivisionReportsForOtherGrade();
		System.out.println("IesaSampleData.main() : "+reports.iterator().next().getClassroomSessionDivisionSubjects().size());
	}
	
}
