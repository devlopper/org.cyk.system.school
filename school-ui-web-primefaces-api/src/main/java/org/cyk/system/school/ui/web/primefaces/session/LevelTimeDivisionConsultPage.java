package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.business.impl.session.LevelTimeDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.ui.web.primefaces.session.AbstractClassLevelConsultPage.BroadsheetColumnAdapter;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelTimeDivisionConsultPage extends AbstractConsultPage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<LevelTimeDivisionDetails> details;
	protected Table<StudentClassroomSessionDetails> broadsheetTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		details = createDetailsForm(LevelTimeDivisionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<LevelTimeDivision,LevelTimeDivisionDetails>(LevelTimeDivision.class, LevelTimeDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		broadsheetTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>(StudentClassroomSession.class, StudentClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSession> getIdentifiables() {
				Collection<StudentClassroomSession> studentClassroomSessions = SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().findByLevelTimeDivision(identifiable);
				RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
		        rankOptions.setType(RankType.EXAEQUO); 
		        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
				SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().computeRank(studentClassroomSessions, rankOptions);
				return studentClassroomSessions;
			}
		});
		
		broadsheetTable.getColumnListeners().add(new BroadsheetColumnAdapter<ClassroomSessionDivision>(userSession, null, Boolean.FALSE, ClassroomSessionDivision.class
				, null, 2, StudentClassroomSessionDetails.FIELDS_BROAD_SHEET){

			private static final long serialVersionUID = -5015534982576207514L;

			@Override
			public Boolean isColumn(Field field) {
				if(StudentClassroomSessionDetails.FIELD_EVALUATION_AVERAGE_DIVIDEND.equals(field.getName()))
					return Boolean.FALSE;
				return super.isColumn(field);
			}
		});
					
	}

}
