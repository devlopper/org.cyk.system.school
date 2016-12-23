package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.TimeDivisionType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity @Getter @Setter @NoArgsConstructor
public class LevelTimeDivision extends AbstractIdentifiable implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	@ManyToOne @NotNull private Level level;
	
	@ManyToOne @NotNull private TimeDivisionType timeDivisionType;

	public LevelTimeDivision(String code,Level level, TimeDivisionType timeDivisionType,Long orderNumber) {
		super();
		this.level = level;
		this.timeDivisionType = timeDivisionType;
		this.setCode(code);
		this.getGlobalIdentifierCreateIfNull().setOrderNumber(orderNumber);
	}
	
	@Override
	public String toString() {
		return getCode();
	}
	
	@Override
	public String getUiString() {
		return getCode();
	}
	
	/**/
	
	public static final String FIELD_LEVEL = "level";
	public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	
}
