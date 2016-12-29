package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import static org.cyk.system.root.model.RootConstant.Code.generate;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;

public interface SchoolConstant {

	String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
	/**/
	
	public static class Code implements Serializable {
		private static final long serialVersionUID = 1L;

		public static class JobTitle implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String DIRECTOR_OF_STUDIES = "DIRECTOR_OF_STUDIES"; 
		}
		
		public static class TimeDivisionType implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE = RootConstant.Code.TimeDivisionType.DAY; 
		}
		
		public static class ValueProperties implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.PK,Student.class);
			
			public static String METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT = generate(MetricCollection.class,"VALUE"
					,LevelGroup.KINDERGARTEN,LevelName.K1,Student.class); 
			
		}
		
		public static class UserInterface implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String TAB_BROADSHEET = "school.broadsheet";
			
		}
		
		public static class IntangibleProduct implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String TUITION = "TUITION";
			
		}
		
		public static class LevelGroupType implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String KINDERGARTEN = "KINDERGARTEN";
			public static String PRIMARY = "PRIMARY";
			public static String SECONDARY = "SECONDARY";
			public static String LICENSE = "LICENSE";
			public static String MASTER = "MASTER";
			public static String DOCTORATE = "DOCTORATE";
		}
		
		public static class LevelGroup implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String KINDERGARTEN = "KINDERGARTEN";
			public static String PRIMARY = "PRIMARY";
			public static String PRIMARY_LOWER = generate(PRIMARY,"LOWER");
			public static String PRIMARY_HIGHER = generate(PRIMARY,"HIGHER");
			public static String SECONDARY = "SECONDARY";
			public static String SECONDARY_LOWER = generate(SECONDARY,"LOWER");
			public static String SECONDARY_HIGHER = generate(SECONDARY,"HIGHER");
			public static String LICENSE = "LICENSE";
			public static String MASTER = "MASTER";
			public static String DOCTORATE = "DOCTORATE";
		}
		
		public static class LevelName implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String PK = "PK";
			public static String K1 = "K1";
			public static String K2 = "K2";
			public static String K3 = "K3";
			public static String G1 = "G1";
			public static String G2 = "G2";
			public static String G3 = "G3";
			public static String G4 = "G4";
			public static String G5 = "G5";
			public static String G6 = "G6";
			public static String G7 = "G7";
			public static String G8 = "G8";
			public static String G9 = "G9";
			public static String G10 = "G10";
			public static String G11 = "G11";
			public static String G12 = "G12";
			
		}
		
		public static class Level implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String PK = generate(LevelName.PK);
			public static String K1 = generate(LevelName.K1);
			public static String K2 = generate(LevelName.K2);
			public static String K3 = generate(LevelName.K3);
			public static String G1 = generate(LevelName.G1);
			public static String G2 = generate(LevelName.G2);
			public static String G3 = generate(LevelName.G3);
			public static String G4 = generate(LevelName.G4);
			public static String G5 = generate(LevelName.G5);
			public static String G6 = generate(LevelName.G6);
			public static String G7 = generate(LevelName.G7);
			public static String G8 = generate(LevelName.G8);
			public static String G9 = generate(LevelName.G9);
			public static String G10 = generate(LevelName.G10);
			public static String G11 = generate(LevelName.G11);
			public static String G12 = generate(LevelName.G12);
			
		}
		
		public static class LevelTimeDivision implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String PK_YEAR_1 = generate(Level.PK,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String K1_YEAR_1 = generate(Level.K1,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String K2_YEAR_1 = generate(Level.K2,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String K3_YEAR_1 = generate(Level.K3,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G1_YEAR_1 = generate(Level.G1,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G2_YEAR_1 = generate(Level.G2,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G3_YEAR_1 = generate(Level.G3,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G4_YEAR_1 = generate(Level.G4,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G5_YEAR_1 = generate(Level.G5,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G6_YEAR_1 = generate(Level.G6,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G7_YEAR_1 = generate(Level.G7,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G8_YEAR_1 = generate(Level.G8,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G9_YEAR_1 = generate(Level.G9,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G10_YEAR_1 = generate(Level.G10,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G11_YEAR_1 = generate(Level.G11,RootConstant.Code.TimeDivisionType.YEAR,"1");
			public static String G12_YEAR_1 = generate(Level.G12,RootConstant.Code.TimeDivisionType.YEAR,"1");
			//public static String G13_YEAR_1 = generate(Level.G13,RootConstant.Code.TimeDivisionType.YEAR,"1");
			
		}
		
		public static class Subject implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ACCOUNTING = "ACCOUNTING ";
			public static String ADVANCED_MATHEMATICS = "ADVANCED_MATHEMATICS ";
			public static String ART_CRAFT = "ART_CRAFT ";
			public static String ART_DESIGN = "ART_DESIGN ";
			public static String BIOLOGY = "BIOLOGY ";
			public static String BUSINESS_STUDIES = "BUSINESS_STUDIES ";
			public static String CHECKPOINT_ENGLISH_LEVEL = "CHECKPOINT_ENGLISH_LEVEL ";
			public static String CHECKPOINT_MATHEMATICS = "CHECKPOINT_MATHEMATICS ";
			public static String CHECKPOINT_SCIENCES = "CHECKPOINT_SCIENCES ";
			public static String CHEMISTRY = "CHEMISTRY ";
			public static String CHINESE_MANDARIN = "CHINESE_MANDARIN ";
			public static String COMPREHENSION = "COMPREHENSION ";
			public static String CORE_MATHEMATICS = "CORE_MATHEMATICS ";
			public static String CREATIVE_WRITING = "CREATIVE_WRITING "; 
			public static String CREATIVITY_ACTIVITY_SERVICE = "CREATIVITY_ACTIVITY_SERVICE ";
			public static String DEVELOPMENT_STUDIES = "DEVELOPMENT_STUDIES ";
			public static String EARTH_SCIENCES = "EARTH_SCIENCES ";
			public static String ECONOMICS = "ECONOMICS ";
			public static String ENGLISH_FIRST_LANGUAGE = "ENGLISH_FIRST_LANGUAGE ";
			public static String ENGLISH_LANGUAGE = "ENGLISH_LANGUAGE ";
			public static String ENGLISH_LITERATURE = "ENGLISH_LITERATURE ";
			public static String ENVIRONMENTAL_MANAGEMENT = "ENVIRONMENTAL_MANAGEMENT ";
			public static String EXTENDED_ESSAY = "EXTENDED_ESSAY ";
			public static String EXTENDED_MATHEMATICS = "EXTENDED_MATHEMATICS ";
			public static String FRENCH = "FRENCH ";
			public static String FRENCH_FOREIGN_LANGUAGE = "FRENCH_FOREIGN_LANGUAGE ";
			public static String GENERAL_KNOWLEDGE = "GENERAL_KNOWLEDGE ";
			public static String GEOGRAPHY = "GEOGRAPHY ";
			public static String GRAMMAR = "GRAMMAR ";
			public static String HANDWRITING = "HANDWRITING ";
			public static String HISTORY = "HISTORY ";
			public static String HOME_ECONOMICS = "HOME_ECONOMICS ";
			public static String ICT_COMPUTER = "ICT_COMPUTER ";
			public static String LAW = "LAW ";
			public static String LITERATURE = "LITERATURE ";
			public static String LITERATURE_IN_ENGLISH = "LITERATURE_IN_ENGLISH ";
			public static String MATHEMATICS = "MATHEMATICS ";
			public static String MORAL_EDUCATION = "MORAL_EDUCATION ";
			public static String MUSIC = "MUSIC ";
			public static String PHONICS = "PHONICS ";
			public static String PHYSICAL_EDUCATION = "PHYSICAL_EDUCATION ";
			public static String PHYSICS = "PHYSICS ";
			public static String READING_COMPREHENSION = "READING_COMPREHENSION ";
			public static String SCIENCE = "SCIENCE ";
			public static String SOCIAL_STUDIES = "SOCIAL_STUDIES ";
			public static String SOCIOLOGY = "SOCIOLOGY ";
			public static String STEM = "STEM ";
			public static String SPANISH = "SPANISH ";
			public static String SPANISH_FOREIGN_LANGUAGE = "SPANISH_FOREIGN_LANGUAGE ";
			public static String SPELLING = "SPELLING ";
			public static String THEORY_OF_KNOWLEDGE = "THEORY_OF_KNOWLEDGE ";
			public static String UCMAS = "UCMAS ";
			
		}
		
		public static class ClassroomSessionSuffix implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String A = "A ";
			public static String B = "B ";
			public static String C = "C ";
			
		}
		
		public static class MetricCollectionType implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE_STUDENT = generate(RootConstant.Code.MetricCollectionType.ATTENDANCE,Student.class); 
			public static String BEHAVIOUR_STUDENT = generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,Student.class);
			public static String COMMUNICATION_STUDENT = generate(RootConstant.Code.MetricCollectionType.COMMUNICATION,Student.class); 
			
			public static String BEHAVIOUR_KINDERGARTEN_STUDENT = generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.KINDERGARTEN,Student.class); 
			public static String BEHAVIOUR_PRIMARY_STUDENT = generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			public static String BEHAVIOUR_SECONDARY_STUDENT = generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
		
			public static final Collection<String> _STUDENT = new ArrayList<>(Arrays.asList(ATTENDANCE_STUDENT,BEHAVIOUR_STUDENT,COMMUNICATION_STUDENT));
		}
		
		public static class MetricCollection implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE_STUDENT = generate(RootConstant.Code.MetricCollection.ATTENDANCE,Student.class); 
			public static String ATTENDANCE_KINDERGARTEN_STUDENT = generate(RootConstant.Code.MetricCollection.ATTENDANCE
					,LevelGroup.KINDERGARTEN,Student.class); 
			
			public static String BEHAVIOUR_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,Student.class);
			public static String COMMUNICATION_STUDENT = generate(RootConstant.Code.MetricCollection.COMMUNICATION,Student.class); 
			public static String COMMUNICATION_KINDERGARTEN_STUDENT = generate(RootConstant.Code.MetricCollection.COMMUNICATION
					,LevelGroup.KINDERGARTEN,Student.class); 
			
			//public static String BEHAVIOUR_KINDERGARTEN_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,Student.class); 
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.K1,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.K2,Student.class);
			
			public static String BEHAVIOUR_PRIMARY_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			public static String BEHAVIOUR_SECONDARY_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
			
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"EXPRESSIVELANGUAGE");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"RECEPTIVELANGUAGE");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"READINGREADNESS");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"NUMERACYDEVELOPMENT");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"ARTSMUSIC");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"SOCIALEMOTIONALDEVELOPMENT");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"GROSSMOTORSKILLS");
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class,"FINEMOTORSKILLS");
			
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING = "MCK1SELAR";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS = "MCK1SCS";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE = "MCK1SS";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES = "MCK1SSS";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS = "MCK1SM";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS = "MCK1SWH";
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS = "MCK1SSSK";
			public static final Collection<String> BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ = new ArrayList<>(Arrays.asList(
					BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS
					,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS
					,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS));
			public static final Collection<String> BEHAVIOUR_KINDERGARTEN_K1_STUDENT_EVALUATED = new ArrayList<>(Arrays.asList(
					BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS
					,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES,BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS));
			
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS = "MCK2K3SRR";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING = "MCK2K3SR";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING = "MCK2K3SW";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING = "MCK2K3SLSV";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION = "MCK2K3SAI";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS = "MCK2K3SM";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION = "MCK2K3SSSSME";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT = "MCK2K3SAC";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC = "MCK2K3SMM";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION = "MCK2K3SPE";
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS = "MCK2K3SWBH";
		}
		
		public static class Metric implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE_NUMBER_OF_TIME_ABSENT_STUDENT = generate("NUMBEROFTIMEABSENT",Student.class);
			public static String ATTENDANCE_NUMBER_OF_TIME_PRESENT_STUDENT = generate("NUMBEROFTIMEPRESENT",Student.class); 
			public static String ATTENDANCE_NUMBER_OF_TIME_DETENTION_STUDENT = generate("NUMBEROFTIMEDETENTION",Student.class); 
			public static String ATTENDANCE_NUMBER_OF_TIME_SUSPENDED_STUDENT = generate("NUMBEROFTIMESUSPENDED",Student.class); 
		}
		
		public static class IntervalCollection implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.PK,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.K1,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.K2,Student.class);
			
			public static String BEHAVIOUR_PRIMARY_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			
			public static String BEHAVIOUR_SECONDARY_STUDENT = generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
			
			public static String METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT = generate(MetricCollection.class,"VALUE"
					,LevelGroup.KINDERGARTEN,LevelName.K1,Student.class); 
			
			public static String GRADING_SCALE_PRIMARY_STUDENT = generate("GRADINGSCALE",LevelGroup.PRIMARY,Student.class);
			public static String GRADING_SCALE_SECONDARY_STUDENT = generate("GRADINGSCALE",LevelGroup.SECONDARY,Student.class);
			
			public static String PROMOTION_SCALE_STUDENT = generate("PROMOTIONSCALE",Student.class);
		}
		
		public static class Interval implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String EVALUATION_COUNT_BY_TYPE = generate(Evaluation.class,"COUNTBY",ClassroomSessionDivisionSubjectEvaluationType.class);
			public static String DIVISION_COUNT_BY_CLASSROOM_SESSION = generate(ClassroomSessionDivision.class,"COUNTBY",ClassroomSession.class);
		}
		
		public static class ReportTemplate implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String STUDENT_REGISTRATION_CERTIFICATE = "STUDENT_REGISTRATION_CERTIFICATE";
			public static String STUDENT_TUITION_CERTIFICATE = "STUDENT_TUITION_CERTIFICATE";
			
			public static String STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_KINDERGARTEN_PK = generate(ClassroomSessionDivision.class
					,"Results",LevelGroup.KINDERGARTEN,LevelName.PK);
			public static String CLASSROOM_SESSION_DIVISION_RESULTS_KINDERGARTEN_K1_STUDENT = generate(ClassroomSessionDivision.class
					,"Results",LevelGroup.KINDERGARTEN,LevelName.K1,Student.class);
			public static String CLASSROOM_SESSION_DIVISION_RESULTS_KINDERGARTEN_K2_STUDENT = generate(ClassroomSessionDivision.class
					,"Results",LevelGroup.KINDERGARTEN,LevelName.K2,Student.class);
			public static String CLASSROOM_SESSION_DIVISION_RESULTS_KINDERGARTEN_K3_STUDENT = generate(ClassroomSessionDivision.class
					,"Results",LevelGroup.KINDERGARTEN,LevelName.K3,Student.class);
			public static String STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS = generate(StudentClassroomSessionDivision.class,"Results");
		}
		
		public static class Role implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String TEACHER = "TEACHER";
			public static String STUDENT = "STUDENT";
			
		}
		
		public static class EvaluationType implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String TEST = "TEST";
			public static String TEST1 = "TEST1";
			public static String TEST2 = "TEST2";
			
			public static String EXAM = "EXAM";
			
			public static final Collection<String> COLLECTION = new ArrayList<>();
			
		}
		
		public static class ValueCollection implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_STUDENT = "STUDENTCLASSROOMSESSIONDIVISIONRESULTSSTUDENT";
			public static String STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_ATTENDANCE = "STUDENTCLASSROOMSESSIONDIVISIONRESULTSATTENDANCE";
			
		}
		
	}
	
	public static class Configuration {
		
		public static class Evaluation implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static Boolean COEFFICIENT_APPLIED = Boolean.FALSE;
			
			
		}
		
	}

}
