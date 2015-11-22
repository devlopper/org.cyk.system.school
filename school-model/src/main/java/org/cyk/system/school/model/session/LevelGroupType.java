package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.pattern.tree.DataTreeType;

@Getter @Setter @NoArgsConstructor @Entity 
public class LevelGroupType extends DataTreeType implements Serializable  {

	private static final long serialVersionUID = -6838401709866343401L;

	public LevelGroupType(DataTreeType parent, String code,String label) {
		super(parent, code,label);
	}

	
	
}
