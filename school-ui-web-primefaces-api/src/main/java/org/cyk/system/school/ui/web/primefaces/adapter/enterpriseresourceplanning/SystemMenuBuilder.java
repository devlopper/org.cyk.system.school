package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelSpeciality;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.UserSession;

public class SystemMenuBuilder extends org.cyk.system.company.ui.web.primefaces.adapter.enterpriseresourceplanning.SystemMenuBuilder implements Serializable {

	private static final long serialVersionUID = 6995162040038809581L;

	private static SystemMenuBuilder INSTANCE;
	
	@Override
	public SystemMenu build(UserSession userSession) {
		SystemMenu systemMenu = super.build(userSession);
		addBusinessMenu(userSession,systemMenu,getStudentCommandable(userSession, null));
		return systemMenu;
	}
	
	public Commandable getAcademicCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("command.academic.management", null);
		module.addChild(createListCommandable(AcademicSession.class, null));
		module.addChild(createListCommandable(LevelTimeDivision.class, null));
		module.addChild(createListCommandable(ClassroomSession.class, null));
		module.addChild(createListCommandable(ClassroomSessionDivision.class, null));
		module.addChild(createListCommandable(ClassroomSessionDivisionSubject.class, null));
		
		module.addChild(createListCommandable(Evaluation.class, null));
		module.addChild(createListCommandable(StudentClassroomSessionDivision.class, null));
		
		module.addChild(createListCommandable(Subject.class, null));
		return module;
	}
	
	public Commandable getStudentCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("command.student.management", null);
		module.addChild(createListCommandable(Student.class, null));
		module.addChild(createListCommandable(StudentClassroomSession.class, null));
		addReportCommandables(Student.class,module, StudentBusinessImpl.Listener.COLLECTION);
		return module;
	}
	
	public Commandable getLevelCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable(Level.class, null);
		module.addChild(createListCommandable(LevelName.class, null));
		module.addChild(createListCommandable(LevelSpeciality.class, null));
		return module;
	}
	
	@Override
	public Commandable getEmployeeCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables) {
		Commandable module = super.getEmployeeCommandable(userSession, mobileCommandables);
		module.addChild(createListCommandable(Teacher.class, null));
		addReportCommandables(Teacher.class,module, TeacherBusinessImpl.Listener.COLLECTION);
		return module;
	}
	
	public static SystemMenuBuilder getInstance(){
		if(INSTANCE==null)
			INSTANCE = new SystemMenuBuilder();
		return INSTANCE;
	}
}
