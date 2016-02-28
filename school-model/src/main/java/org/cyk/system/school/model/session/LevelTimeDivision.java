package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Entity @Getter @Setter @NoArgsConstructor
public class LevelTimeDivision extends AbstractIdentifiable implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo @NotNull private Level level;
	
	@ManyToOne @Input @InputChoice @InputOneChoice @InputOneCombo @NotNull private TimeDivisionType timeDivisionType;

 	@Column(name="theindex",nullable=false) @NotNull private Integer index;
	
	public LevelTimeDivision(Level level, TimeDivisionType timeDivisionType,Integer index) {
		super();
		this.level = level;
		this.timeDivisionType = timeDivisionType;
		this.index = index;
	}
	
	@Override
	public String getUiString() {
		return level.getUiString()+" "+timeDivisionType.getUiString();
	}
	
	/**/
	
	public static final String FIELD_INDEX = "index";
	
}
