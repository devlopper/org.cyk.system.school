package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public abstract class AbstractLevelTimeDivisionConsultPage extends AbstractConsultPage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentClassroomSessionDetails> broadsheetTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		broadsheetTable = (Table<StudentClassroomSessionDetails>) createDetailsTable(StudentClassroomSessionDetails.class, new StudentClassroomSessionConsultManyRankPage.TableAdapter() {
			private static final long serialVersionUID = -2065739751466832899L;
			@Override
			protected Collection<StudentClassroomSession> getStudentClassroomSessions() {
				return inject(StudentClassroomSessionBusiness.class).findByLevelTimeDivision(identifiable);
			}
		});
		
		List<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(StudentClassroomSessionConsultManyRankPage.filterClassroomSessionDivisions(inject(ClassroomSessionDivisionBusiness.class).findByLevelTimeDivision(identifiable)));
			
		broadsheetTable.getColumnListeners().add(new StudentClassroomSessionConsultManyRankPage.ColumnAdapter(userSession, classroomSessionDivisions));
		
		broadsheetTable.getCellListeners().add(new StudentClassroomSessionConsultManyRankPage.CellAdpater(classroomSessionDivisions) {
			
			private static final long serialVersionUID = -4986730778696471541L;

			@Override
			protected Collection<StudentClassroomSessionDivision> getDetailCollection() {
				return inject(StudentClassroomSessionDivisionBusiness.class).findByLevelTimeDivision(identifiable);
			}
		});
		
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(LevelTimeDivision.class);
	}

}
