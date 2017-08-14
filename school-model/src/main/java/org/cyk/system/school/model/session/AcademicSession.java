package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.root.model.time.AbstractIdentifiablePeriod;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.FEMALE)
public class AcademicSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne @JoinColumn(name=COLUMN_SCHOOL) @NotNull private School school;
		
	@Embedded private CommonNodeInformations nodeInformations;
	
	@Transient private IdentifiableRuntimeCollection<ClassroomSession> classroomSessions = new IdentifiableRuntimeCollection<>();
	@Transient private IdentifiableRuntimeCollection<LevelGroup> levelGroups = new IdentifiableRuntimeCollection<>();
	@Transient private IdentifiableRuntimeCollection<LevelName> levelNames = new IdentifiableRuntimeCollection<>();
	
	public AcademicSession(School school,CommonNodeInformations nodeInformations) {
		super();
		this.school = school;
		this.nodeInformations = nodeInformations;
	} 
	
	public CommonNodeInformations getNodeInformations(){
		if(nodeInformations==null)
			nodeInformations = new CommonNodeInformations();
		return nodeInformations;
	}
	
	@Override
	public String getUiString() {
		return getCode();
	}
	
	@Override
	public String toString() {
		return getCode();
	}
	
	/**/
	
	public static final String FIELD_SCHOOL = "school";
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
	
	public static final String COLUMN_SCHOOL = "school";
}
