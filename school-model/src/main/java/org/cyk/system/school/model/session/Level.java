package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
public class Level extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private LevelGroup group;
	@ManyToOne private LevelName levelName;
	@ManyToOne private LevelSpeciality speciality;
	
	public Level(LevelGroup group,LevelName levelName, LevelSpeciality speciality) {
		super();
		this.group = group;
		this.levelName = levelName;
		this.speciality = speciality;
	}
	
	public Level(LevelGroup group,LevelName name) {
		this(group,name,null);
	}
	
	@Override
	public String toString() {
		return levelName+(speciality==null?Constant.EMPTY_STRING:(Constant.CHARACTER_SPACE.toString()+speciality));
	}
	
	@Override
	public String getUiString() {
		return toString();
	}
	
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_LEVEL_NAME = "levelName";
	public static final String FIELD_LEVEL_SPECIALITY = "speciality";
}
