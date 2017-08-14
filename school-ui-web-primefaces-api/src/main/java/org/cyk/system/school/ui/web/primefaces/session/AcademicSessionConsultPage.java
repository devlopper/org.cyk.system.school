package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.api.IdentifierProvider;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionConsultPage extends AbstractConsultPage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionDetails> classroomSessionTable;
	
	@Override
	protected void consultInitialisation() {
		super.consultInitialisation();
		classroomSessionTable = (Table<ClassroomSessionDetails>) createDetailsTable(ClassroomSessionDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSession,ClassroomSessionDetails>(ClassroomSession.class, ClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSession> getIdentifiables() {
				return inject(ClassroomSessionBusiness.class).findByAcademicSession(identifiable);
			}
			
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			
			@Override
			public String getTabId() {
				return IdentifierProvider.Adapter.getTabOf(AcademicSession.class);
			}
			
			@Override
			public String getEditPageOutcome() {
				return "academicSessionEditClassroomSessionsView";
			}
			
			@Override
			public AbstractIdentifiable getFormIdentifiable() {
				return identifiable;
			}
			
		});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		((Commandable)classroomSessionTable.getUpdateCommandable()).setRendered(Boolean.TRUE);
	}
}
