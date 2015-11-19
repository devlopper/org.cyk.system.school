package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Getter @Setter @Entity @NoArgsConstructor
public class Level extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo
	private LevelName name;
	
	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo
	private LevelSpeciality speciality;

	public Level(LevelName name, LevelSpeciality speciality) {
		super();
		this.name = name;
		this.speciality = speciality;
	}
	
	public Level(LevelName name) {
		this(name,null);
	}
	
	@Override
	public String getUiString() {
		return name+(speciality==null?"":(" "+speciality));
	}
	
}
