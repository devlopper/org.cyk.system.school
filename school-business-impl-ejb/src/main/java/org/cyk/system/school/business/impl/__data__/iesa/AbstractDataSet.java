package org.cyk.system.school.business.impl.__data__.iesa;

import java.io.Serializable;

import org.cyk.system.school.model.SchoolConstant;

public abstract class AbstractDataSet extends org.cyk.system.school.business.impl.__data__.DataSet implements Serializable {

	private static final long serialVersionUID = 2282674526022995453L;
	
	public AbstractDataSet() {
		addClassroomSessionSuffixes(new Object[][]{
			{SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1,new String[]{null}}
		});
		
		getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1);
    	getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1);
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1}, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT}
				,{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HANDWRITING}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.PHONICS}
    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.READING_COMPREHENSION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SCIENCE}
    			,{SchoolConstant.Code.Subject.UCMAS},{SchoolConstant.Code.Subject.SPELLING}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1}, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT}
				,{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR}
    			,{SchoolConstant.Code.Subject.HISTORY},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE}
    			,{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.MORAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC}
    			,{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SCIENCE}
    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS}
				,{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES}
    			,{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.FRENCH}
    			,{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_ENGLISH_LEVEL}
				,{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS}
    			,{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES},{SchoolConstant.Code.Subject.DIVINITY},{SchoolConstant.Code.Subject.EARTH_SCIENCES}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.HOME_ECONOMICS},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES}
    			,{SchoolConstant.Code.Subject.SPANISH},{SchoolConstant.Code.Subject.STEM},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ACCOUNTING},{SchoolConstant.Code.Subject.ART_DESIGN},{SchoolConstant.Code.Subject.BIOLOGY}
    			,{SchoolConstant.Code.Subject.BUSINESS_STUDIES},{SchoolConstant.Code.Subject.CHEMISTRY},{SchoolConstant.Code.Subject.CREATIVITY_ACTIVITY_SERVICE}
    			,{SchoolConstant.Code.Subject.DEVELOPMENT_STUDIES},{SchoolConstant.Code.Subject.ECONOMICS},{SchoolConstant.Code.Subject.ENGLISH_FIRST_LANGUAGE}
    			,{SchoolConstant.Code.Subject.ENVIRONMENTAL_MANAGEMENT},{SchoolConstant.Code.Subject.EXTENDED_ESSAY},{SchoolConstant.Code.Subject.EXTENDED_MATHEMATICS}
    			,{SchoolConstant.Code.Subject.FRENCH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LAW},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.PHYSICS},{SchoolConstant.Code.Subject.SOCIOLOGY}
    			,{SchoolConstant.Code.Subject.SPANISH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.THEORY_OF_KNOWLEDGE}});
		
		addClassroomSessionEvaluationTypes(new Object[][]{
			{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
			,{SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1,new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
				,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}}
		});
		
		setExcelWorkbookFileName(null);
		
	}
		
}
