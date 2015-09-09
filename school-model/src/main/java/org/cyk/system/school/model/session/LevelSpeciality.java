package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractEnumeration;

@Entity
@Getter @Setter
public class LevelSpeciality extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	public LevelSpeciality() {}

	public LevelSpeciality(String code,String name, String abbreviation) {
		super(code,name, abbreviation,null);
	}
	
}
