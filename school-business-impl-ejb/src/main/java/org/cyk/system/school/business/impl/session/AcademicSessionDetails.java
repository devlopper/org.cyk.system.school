package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class AcademicSessionDetails extends AbstractOutputDetails<AcademicSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private FieldValue school;
	@IncludeInputs private CommonNodeInformationsDetails nodeInformations;
	
	public AcademicSessionDetails(AcademicSession academicSession) {
		super(academicSession);
	}
	
	@Override
	public void setMaster(AcademicSession academicSession) {
		super.setMaster(academicSession);
		if(academicSession!=null){
			school = new FieldValue(academicSession.getSchool());
			if(nodeInformations==null)
				nodeInformations = new CommonNodeInformationsDetails(academicSession.getNodeInformations());
			else
				nodeInformations.setMaster(academicSession.getNodeInformations());
		}
	}
	
	public static final String FIELD_SCHOOL = "school";
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";

}