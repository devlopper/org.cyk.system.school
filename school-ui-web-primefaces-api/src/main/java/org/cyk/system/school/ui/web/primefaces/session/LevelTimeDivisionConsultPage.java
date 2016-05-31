package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.LevelTimeDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelTimeDivisionConsultPage extends AbstractConsultPage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<LevelTimeDivisionDetails> details;
	private Table<StudentClassroomSessionDetails> broadsheetTable;
	
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
		
		broadsheetTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new StudentClassroomSessionConsultManyRankPage.TableAdapter() {
			private static final long serialVersionUID = -2065739751466832899L;
			@Override
			protected Collection<StudentClassroomSession> getStudentClassroomSessions() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().findByLevelTimeDivision(identifiable);
			}
		});
		
		List<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(StudentClassroomSessionConsultManyRankPage.filterClassroomSessionDivisions(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByLevelTimeDivision(identifiable)));
			
		broadsheetTable.getColumnListeners().add(new StudentClassroomSessionConsultManyRankPage.ColumnAdapter(userSession, classroomSessionDivisions));
		
		broadsheetTable.getCellListeners().add(new StudentClassroomSessionConsultManyRankPage.CellAdpater(classroomSessionDivisions) {
			
			private static final long serialVersionUID = -4986730778696471541L;

			@Override
			protected Collection<StudentClassroomSessionDivision> getDetailCollection() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByLevelTimeDivision(identifiable);
			}
		});
	
	}

}
