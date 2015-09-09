package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractEnumeration;

@Entity
@Getter @Setter
public class EvaluationTypeName extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
	public EvaluationTypeName() {}

	public EvaluationTypeName(String code,String name, String abbreviation) {
		super(code,name, abbreviation,null);
	}
	
}
