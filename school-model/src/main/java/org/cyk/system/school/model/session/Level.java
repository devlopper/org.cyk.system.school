package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import org.cyk.system.root.model.AbstractIdentifiable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
public class Level extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private LevelGroup group;
	@ManyToOne private LevelName name;
	@ManyToOne private LevelSpeciality speciality;
	
	public Level(LevelGroup group,LevelName name, LevelSpeciality speciality) {
		super();
		this.group = group;
		this.name = name;
		this.speciality = speciality;
	}
	
	public Level(LevelGroup group,LevelName name) {
		this(group,name,null);
	}
	
	@Override
	public String toString() {
		return name+(speciality==null?"":(" "+speciality));
	}
	
	@Override
	public String getUiString() {
		return toString();
	}
	
}
