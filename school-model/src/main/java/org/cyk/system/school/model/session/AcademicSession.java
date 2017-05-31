package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
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
	
	@ManyToOne @NotNull private School school;
		
	@Embedded private CommonNodeInformations nodeInformations;
	
	@Temporal(value=TemporalType.DATE) private Date nextStartingDate;//TODO to be deleted. look for the next to get the from date
	
	@Transient private IdentifiableRuntimeCollection<LevelGroup> levelGroups = new IdentifiableRuntimeCollection<>();
	@Transient private IdentifiableRuntimeCollection<LevelName> levelNames = new IdentifiableRuntimeCollection<>();
	
	public AcademicSession(School school,CommonNodeInformations nodeInformations,Date nextStartingDate) {
		super();
		this.school = school;
		this.nodeInformations = nodeInformations;
		this.nextStartingDate = nextStartingDate;
	} 
	
	public CommonNodeInformations getNodeInformations(){
		if(nodeInformations==null)
			nodeInformations = new CommonNodeInformations();
		return nodeInformations;
	}
	
	/**/
	
	public static final String FIELD_SCHOOL = "school";
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
	public static final String FIELD_NEXT_STARTING_DATE = "nextStartingDate";
	
}
