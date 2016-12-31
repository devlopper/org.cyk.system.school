package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractEnumeration;

@Entity @Getter @Setter
public class LevelName extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	@Embedded private CommonNodeInformations nodeInformations;
	
	public LevelName() {}

	public LevelName(String code,String name, String abbreviation,CommonNodeInformations nodeInformations) {
		super(code,name, abbreviation,null);
		this.nodeInformations = nodeInformations;
	}
	
	public CommonNodeInformations getNodeInformations(){
		if(nodeInformations==null)
			nodeInformations = new CommonNodeInformations();
		return nodeInformations;
	}
	
}
