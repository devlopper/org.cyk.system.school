package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;

import org.cyk.system.root.model.pattern.tree.AbstractDataTree;
import org.cyk.utility.common.annotation.FieldOverride;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor @Entity @FieldOverride(name=LevelGroup.FIELD_TYPE,type=LevelGroupType.class)
public class LevelGroup extends AbstractDataTree<LevelGroupType> implements Serializable  {

	private static final long serialVersionUID = -6128937819261060725L;

	@Embedded private CommonNodeInformations nodeInformations = new CommonNodeInformations();
	
	public LevelGroup(AbstractDataTree<LevelGroupType> parent, LevelGroupType type, String code,String name) {
		super(parent, type, code);
		setName(name);
	}
	
	public LevelGroup(AbstractDataTree<LevelGroupType> parent, LevelGroupType type, String code) {
		super(parent,type,code);
	}
	
	public LevelGroup setNodeInformations(CommonNodeInformations nodeInformations){
		this.nodeInformations = nodeInformations;
		return this;
	}
	
	public CommonNodeInformations getNodeInformations(){
		if(nodeInformations==null)
			nodeInformations = new CommonNodeInformations();
		return nodeInformations;
	}
}
