package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;

import org.cyk.system.root.model.AbstractEnumeration;

import lombok.Getter;
import lombok.Setter;

@Entity @Getter @Setter
public class ClassroomSessionSuffix extends AbstractEnumeration implements Serializable{
	
	private static final long serialVersionUID = 374208919427476791L;
	
}
