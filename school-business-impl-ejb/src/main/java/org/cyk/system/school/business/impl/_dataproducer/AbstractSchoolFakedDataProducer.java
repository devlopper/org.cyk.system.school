package org.cyk.system.school.business.impl._dataproducer;
import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.AbstractCompanyFakedDataProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Deprecated
public abstract class AbstractSchoolFakedDataProducer extends AbstractCompanyFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Accessors(chain=true) @Setter protected Set<String> classroomSessionLevelTimeDivisionCodes = new LinkedHashSet<>();
	@Accessors(chain=true) @Setter protected Set<Long> divisionOrderNumbers = new LinkedHashSet<>();
	@Accessors(chain=true) @Setter protected Map<String,String[]> classroomSessionSuffixes = new HashMap<>();
	@Accessors(chain=true) @Setter protected Map<String,String[][]> classroomSessionSubjects = new HashMap<>();
	@Accessors(chain=true) @Setter protected Map<String,Object[][]> studentMap = new HashMap<>();

	@Override
	protected Package getBasePackage() {
		return SchoolBusinessLayer.class.getPackage();
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
	
	protected void addClassroomSessionSuffixes(Object[][] values){
		for(Object[] array : values)
			classroomSessionSuffixes.put((String)array[0], (String[])array[1]);
	}
	
	protected String[] getClassroomSessionSuffixes(String levelTimeDivisionCode){
		return classroomSessionSuffixes.get(levelTimeDivisionCode);
	}
	
	protected void addStudent(String levelTimeDivisionCode,String classroomSessionSuffixCode,Object[][] values){
		studentMap.put(getClassroomSessionCode(levelTimeDivisionCode, classroomSessionSuffixCode), values);
	}
	
	protected Object[][] getStudents(String levelTimeDivisionCode,String classroomSessionSuffixCode){
		return studentMap.get(getClassroomSessionCode(levelTimeDivisionCode, classroomSessionSuffixCode));
	}
		
}