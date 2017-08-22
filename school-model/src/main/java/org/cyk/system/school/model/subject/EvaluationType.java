package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;

import org.cyk.system.root.model.AbstractEnumeration;

import lombok.Getter;
import lombok.Setter;

@Entity @Getter @Setter
public class EvaluationType extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;

	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE) private BigDecimal maximum;
	
	public EvaluationType() {}

	public EvaluationType(String code,String name, String abbreviation) {
		super(code,name, abbreviation,null);
	}
	
	public static final String FIELD_MAXIMUM = "maximum";
}
