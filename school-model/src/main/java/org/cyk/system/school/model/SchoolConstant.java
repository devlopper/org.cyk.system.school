package org.cyk.system.school.model;

import java.io.Serializable;

public interface SchoolConstant {

	String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
	String INTANGIBLE_PRODUCT_TUITION = "TUITION";
	
	/**/
	
	String UI_TAB_BROADSHEET_ID = "school.broadsheet";
	
	String REPORT_STUDENT_REGISTRATION_CERTIFICATE = "STUDENT_REGISTRATION_CERTIFICATE";
	String REPORT_STUDENT_TUITION_CERTIFICATE = "STUDENT_TUTION_CERTIFICATE";
	//String REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET = "STUDENT_CLASSROOM_SESSION_DIVISION_SHEET";

	/**/
	
	public static class Code implements Serializable {
		private static final long serialVersionUID = 1L;

		public static class LevelGroup implements Serializable {
			private static final long serialVersionUID = 1L;
			
			public static String KINDERGARTEN = "KINDERGARTEN";
			public static String PRIMARY = "PRIMARY";
			public static String SECONDARY = "SECONDARY";
		}
		
	}

}
