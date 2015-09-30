package org.cyk.system.school.ui.web.primefaces.registration;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.ui.web.primefaces.page.tools.MedicalInformationsEditPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Getter @Setter
public class MedicalInformationsFormModel extends MedicalInformationsEditPage.FormModel implements Serializable{
	private static final long serialVersionUID = 2231017067376454414L;
	
	@Input @InputText protected String f1;
	@Input @InputText protected String f2;
	@Input @InputText protected String f3;
	
	//@Input @InputText protected String noRecommendationsConcernsOrNeeds;
	//@Input @InputText protected String requestingSchoolFollowUp;
	//@Input @InputText protected String otherMedicalConditionInformations;
	//@Input @InputText protected String doctorPhoneNumber;
}