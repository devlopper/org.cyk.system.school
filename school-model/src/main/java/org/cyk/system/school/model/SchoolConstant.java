package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.cyk.system.root.model.RootConstant;
import org.cyk.system.school.model.actor.Student;

public interface SchoolConstant {

	String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
	/**/
	
	String REPORT_STUDENT_REGISTRATION_CERTIFICATE = "STUDENT_REGISTRATION_CERTIFICATE";
	String REPORT_STUDENT_TUITION_CERTIFICATE = "STUDENT_TUTION_CERTIFICATE";
	//String REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET = "STUDENT_CLASSROOM_SESSION_DIVISION_SHEET";

	/**/
	
	public static class Code implements Serializable {
		private static final long serialVersionUID = 1L;

		public static class JobTitle implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String DIRECTOR_OF_STUDIES = "DIRECTOR_OF_STUDIES"; 
			
		}
		
		public static class ValueProperties implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT = RootConstant.Code.generate(MetricCollection.class,"VALUE"
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
			public static String PRIMARY_LOWER = RootConstant.Code.generate(PRIMARY,"LOWER");
			public static String PRIMARY_HIGHER = RootConstant.Code.generate(PRIMARY,"HIGHER");
			public static String SECONDARY = "SECONDARY";
			public static String SECONDARY_LOWER = RootConstant.Code.generate(SECONDARY,"LOWER");
			public static String SECONDARY_HIGHER = RootConstant.Code.generate(SECONDARY,"HIGHER");
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
		
		public static class MetricCollectionType implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.ATTENDANCE,Student.class); 
			public static String BEHAVIOUR_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,Student.class);
			public static String COMMUNICATION_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.COMMUNICATION,Student.class); 
			
			public static String BEHAVIOUR_KINDERGARTEN_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.KINDERGARTEN,Student.class); 
			public static String BEHAVIOUR_PRIMARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			public static String BEHAVIOUR_SECONDARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollectionType.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
		
			public static final Collection<String> _STUDENT = new ArrayList<>(Arrays.asList(ATTENDANCE_STUDENT,BEHAVIOUR_STUDENT,COMMUNICATION_STUDENT));
		}
		
		public static class MetricCollection implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String ATTENDANCE_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.ATTENDANCE,Student.class); 
			public static String ATTENDANCE_KINDERGARTEN_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.ATTENDANCE
					,LevelGroup.KINDERGARTEN,Student.class); 
			
			public static String BEHAVIOUR_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,Student.class);
			public static String COMMUNICATION_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.COMMUNICATION,Student.class); 
			public static String COMMUNICATION_KINDERGARTEN_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.COMMUNICATION
					,LevelGroup.KINDERGARTEN,Student.class); 
			
			//public static String BEHAVIOUR_KINDERGARTEN_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,Student.class); 
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.PK,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.K1,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN,LevelName.K2,Student.class);
			
			public static String BEHAVIOUR_PRIMARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			public static String BEHAVIOUR_SECONDARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
			
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE = "MCPKSEL";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE = "MCPKSRL";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS = "MCPKSRR";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT = "MCPKSND";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC = "MCPKSAM";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT = "MCPKSSED";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS = "MCPKSGMS";
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS = "MCPKSFMS";
			
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
		
		public static class IntervalCollection implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String BEHAVIOUR_KINDERGARTEN_PK_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.PK,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K1_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.K1,Student.class);
			public static String BEHAVIOUR_KINDERGARTEN_K2_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.KINDERGARTEN
					,LevelName.K2,Student.class);
			
			public static String BEHAVIOUR_PRIMARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.PRIMARY,Student.class);
			
			public static String BEHAVIOUR_SECONDARY_STUDENT = RootConstant.Code.generate(RootConstant.Code.MetricCollection.BEHAVIOUR,LevelGroup.SECONDARY,Student.class);
			
			public static String METRIC_COLLECTION_VALUE_KINDERGARTEN_K1_STUDENT = RootConstant.Code.generate(MetricCollection.class,"VALUE"
					,LevelGroup.KINDERGARTEN,LevelName.K1,Student.class); 
			
			public static String GRADING_SCALE_PRIMARY_STUDENT = RootConstant.Code.generate("GRADINGSCALE",LevelGroup.PRIMARY,Student.class);
			public static String GRADING_SCALE_SECONDARY_STUDENT = RootConstant.Code.generate("GRADINGSCALE",LevelGroup.SECONDARY,Student.class);
			
			public static String PROMOTION_SCALE_STUDENT = RootConstant.Code.generate("PROMOTIONSCALE",Student.class);
		}
		
		public static class ReportTemplate implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String STUDENT_REGISTRATION_CERTIFICATE = "STUDENT_REGISTRATION_CERTIFICATE";
			public static String STUDENT_TUITION_CERTIFICATE = "STUDENT_TUITION_CERTIFICATE";
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
			
		}
		
	}

}
