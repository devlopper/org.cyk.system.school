package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Entity
@Getter @Setter @NoArgsConstructor
public class LevelTimeDivision extends AbstractIdentifiable implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo
	private Level level;
	
	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo
	private TimeDivisionType timeDivisionType;

	public LevelTimeDivision(Level level, TimeDivisionType timeDivisionType) {
		super();
		this.level = level;
		this.timeDivisionType = timeDivisionType;
	}
	
	@Override
	public String getUiString() {
		return level.getUiString()+" "+timeDivisionType.getUiString();
	}
	
}
