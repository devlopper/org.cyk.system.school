package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;

@Getter @Setter
public class SubjectReport extends AbstractIdentifiableReport<Subject> implements Serializable {

	private static final long serialVersionUID = 5049082461268056567L;

	@Override
	public Map<String, List<?>> createFieldsRandomValues() {
		Map<String, List<?>> map = super.createFieldsRandomValues();
		if(map==null){
			map = new HashMap<>();
		}
		map.put(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_NAME), Arrays.asList("Mathematics","Grammar","Reading & Comprehensive"
				,"Handwriting","Spelling","Phonics","Creative writing","Moral education","Social studies","Science","French","Art & Craft","Music","ICT(Computer)"
				,"Physical education"));
		return map;
	}
	
}
