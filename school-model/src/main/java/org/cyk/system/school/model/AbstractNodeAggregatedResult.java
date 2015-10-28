package org.cyk.system.school.model;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.MappedSuperclass;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @MappedSuperclass
public abstract class AbstractNodeAggregatedResult extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@Embedded protected NodeResults results = new NodeResults();
	
	public static final String FIELD_RESULTS = "results";

}
