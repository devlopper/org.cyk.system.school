package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionStudentsMetricCollectionEditPage extends AbstractCrudOnePage<ClassroomSessionDivisionStudentsMetricCollection> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractFormModel<ClassroomSessionDivisionStudentsMetricCollection> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSessionDivision classroomSessionDivision;
		@Input @InputChoice @InputOneChoice @InputOneCombo private MetricCollection metricCollection;
		
		/**/
		
		public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
		public static final String FIELD_METRIC_COLLECTION = "metricCollection";
		
	}

}
