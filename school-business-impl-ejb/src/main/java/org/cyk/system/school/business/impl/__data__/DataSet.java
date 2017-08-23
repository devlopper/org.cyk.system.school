package org.cyk.system.school.business.impl.__data__;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.utility.common.helper.InstanceHelper;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class DataSet extends org.cyk.system.root.business.impl.DataSet implements Serializable {

	private static final long serialVersionUID = 2282674526022995453L;
	
	protected Set<String> classroomSessionLevelTimeDivisionCodes = new LinkedHashSet<>();
	protected Set<Long> divisionOrderNumbers = new LinkedHashSet<>();
	protected Map<String,String[]> classroomSessionSuffixes = new HashMap<>();
	protected Map<String,String[][]> classroomSessionSubjects = new HashMap<>();
	protected Map<String,String[][]> classroomSessionMetricCollections = new HashMap<>();
	protected Map<String,String[][]> classroomSessionEvaluationTypes = new HashMap<>();
	protected Map<String,Object[][]> studentMap = new HashMap<>();
	
	public DataSet() {
		super(SchoolBusinessLayer.class);

		divisionOrderNumbers.add(1l);
		divisionOrderNumbers.add(2l);
		divisionOrderNumbers.add(3l);
		
		addClassroomSessionMetricCollections(new Object[][]{
			{SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION}
				,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
			,{SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
				,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}} }
		});
				
	}
	
	@Override
	public void __instanciate__() {
		super.__instanciate__();
		addInstances(ClassroomSession.class, generateClassroomSessions());
	}
	
	protected Collection<ClassroomSession> generateClassroomSessions(){
		Collection<ClassroomSession> classroomSessions = new ArrayList<>();
		String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
	
    	for(String code : classroomSessionLevelTimeDivisionCodes){
    		for(String suffix : getClassroomSessionSuffixes(code)){
    			classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, getClassroomSessionSubjects(code)
		    		, getClassroomSessionEvaluationTypes(code)
		    		, getClassroomSessionMetricCollections(code)));
	    	}
    	}
    	return classroomSessions;
	}
		
	protected Collection<StudentClassroomSession> generateStudentClassroomSessions(){
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(String levelTimeDivisionCode : classroomSessionLevelTimeDivisionCodes){
			for(String classroomSessionSuffixCode : getClassroomSessionSuffixes(levelTimeDivisionCode)){
				ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class)
						.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(levelTimeDivisionCode,classroomSessionSuffixCode);
				
				Object[][] students = getStudents(levelTimeDivisionCode, classroomSessionSuffixCode);
				if(students!=null){
					for(Object[] studentInfos : students){
						Student student = inject(StudentBusiness.class).instanciateOneRandomly((String)studentInfos[0]);
						student.setName((String)studentInfos[1]);
						student.getPerson().setLastnames((String)studentInfos[2]);
						//student.getPerson().getContactCollection()!=null && student.getPerson().getContactCollection()
						//		.getElectronicMails()!=null);
				    	student.getPerson().getContactCollection().getElectronicMails().clear();
				    	/*if(studentInfos.length>3){
				    		inject(ElectronicMailBusiness.class).setAddress(student.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, (String)studentInfos[3]);
				    		if(studentInfos.length>4){
				    			inject(ElectronicMailBusiness.class).setAddress(student.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, (String)studentInfos[4]);
				    		}
				    	}*/
				    	
				    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{null,classroomSession.getCode()});
				    	studentClassroomSession.setStudent(student);
				    	studentClassroomSessions.add(studentClassroomSession);
				    	
					}		
				}
				
			}
			
		}
		return studentClassroomSessions;
	}
	
	public Collection<Object[]> generate(){
		Collection<Object[]> datas = new ArrayList<>();
		for(String levelTimeDivisionCode : classroomSessionLevelTimeDivisionCodes){
			for(String suffixCode : getClassroomSessionSuffixes(levelTimeDivisionCode)){
				for(Long divisionOrderNumber : divisionOrderNumbers){
					datas.add(new Object[]{levelTimeDivisionCode,suffixCode,divisionOrderNumber});
				}
			}
		}
		return datas;
	}
	
	protected String getClassroomSessionCode(String levelTimeDivisionCode,String classroomSessionSuffixCode){
		return levelTimeDivisionCode+StringUtils.defaultString(classroomSessionSuffixCode);
	}
	
	protected void addClassroomSessionSubjects(String[] levelTimeDivisionCodes,String[][] values){
		for(String levelTimeDivisionCode : levelTimeDivisionCodes)
			classroomSessionSubjects.put(levelTimeDivisionCode, values);
	}
	
	protected String[][] getClassroomSessionSubjects(String levelTimeDivisionCode){
		return classroomSessionSubjects.get(levelTimeDivisionCode);
	}
	
	protected DataSet addClassroomSessionSuffixes(Object[][] values){
		for(Object[] array : values)
			classroomSessionSuffixes.put((String)array[0], (String[])array[1]);
		return this;
	}
	
	protected String[] getClassroomSessionSuffixes(String levelTimeDivisionCode){
		return InstanceHelper.getInstance().getIfNotNullElseDefault(classroomSessionSuffixes.get(levelTimeDivisionCode),new String[]{});
	}
	
	protected DataSet addClassroomSessionMetricCollections(Object[][] values){
		for(Object[] array : values)
			classroomSessionMetricCollections.put((String)array[0], (String[][])array[1]);
		return this;
	}
	
	protected String[][] getClassroomSessionMetricCollections(String levelTimeDivisionCode){
		return InstanceHelper.getInstance().getIfNotNullElseDefault(classroomSessionMetricCollections.get(levelTimeDivisionCode),new String[][]{});
	}
	
	protected DataSet addClassroomSessionEvaluationTypes(Object[][] values){
		for(Object[] array : values)
			classroomSessionEvaluationTypes.put((String)array[0], (String[][])array[1]);
		return this;
	}
	
	protected String[][] getClassroomSessionEvaluationTypes(String levelTimeDivisionCode){
		return InstanceHelper.getInstance().getIfNotNullElseDefault(classroomSessionEvaluationTypes.get(levelTimeDivisionCode),new String[][]{});
	}
	
	protected DataSet addStudent(String levelTimeDivisionCode,String classroomSessionSuffixCode,Object[][] values){
		studentMap.put(getClassroomSessionCode(levelTimeDivisionCode, classroomSessionSuffixCode), values);
		return this;
	}
	
	protected Object[][] getStudents(String levelTimeDivisionCode,String classroomSessionSuffixCode){
		return studentMap.get(getClassroomSessionCode(levelTimeDivisionCode, classroomSessionSuffixCode));
	}
}
