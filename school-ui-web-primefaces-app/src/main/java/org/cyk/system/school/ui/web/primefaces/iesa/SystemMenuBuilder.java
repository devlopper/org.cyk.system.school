package org.cyk.system.school.ui.web.primefaces.iesa;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.sale.Sale;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.company.model.structure.Vehicle;
import org.cyk.system.root.model.message.SmtpProperties;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.AbstractSystemMenuBuilder;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.UserSession;

public class SystemMenuBuilder extends AbstractSystemMenuBuilder implements Serializable {

	private static final long serialVersionUID = 6995162040038809581L;

	private static SystemMenuBuilder INSTANCE;
	
	@Override
	public SystemMenu build(UserSession userSession) {
		SystemMenu systemMenu = new SystemMenu();
		addBusinessMenu(userSession,systemMenu,getStudentCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getEmployeeCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getFinanceCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getAcademicCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getServiceCommandable(userSession, null));
		//addBusinessMenu(userSession,systemMenu,getMessageCommandable(userSession, null));
		return systemMenu;
	}
	
	public Commandable getStudentCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.student.management", null);
		module.addChild(createListCommandable(Student.class, null));
		module.addChild(createListCommandable(StudentClassroomSession.class, null));
		module.addChild(createSelectOneCommandable(StudentClassroomSession.class, inject(SchoolBusinessLayer.class).getActionPrintStudentClassroomSessionTuitionCertificate(), null));
		module.addChild(createSelectOneCommandable(StudentClassroomSession.class, inject(SchoolBusinessLayer.class).getActionPrintStudentClassroomSessionRegistrationCertificate(), null));
		return module;
	}
	
	public Commandable getEmployeeCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.personel.management", null);
		module.addChild(createListCommandable(Employee.class, null));
		module.addChild(createSelectOneCommandable(Employee.class, inject(CompanyBusinessLayer.class).getActionPrintEmployeeEmploymentContract(), null));
		module.addChild(createSelectOneCommandable(Employee.class, inject(CompanyBusinessLayer.class).getActionPrintEmployeeWorkCertificate(), null));
		module.addChild(createSelectOneCommandable(Employee.class, inject(CompanyBusinessLayer.class).getActionPrintEmployeeEmploymentCertificate(), null));
		return module;
	}
	
	public Commandable getFinanceCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.finance.management", null);
		module.addChild(createListCommandable(Sale.class, null));
		
		return module;
	}
	
	public Commandable getAcademicCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.academic.management", null);
		module.addChild(createListCommandable(Subject.class, null));
		
		return module;
	}
	
	public Commandable getServiceCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.service.management", null);
		module.addChild(createListCommandable(Vehicle.class, null));
		
		return module;
	}
	
	public Commandable getMessageCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("commandable.smsmail.management", null);
		module.addChild(createListCommandable(SmtpProperties.class, null));
		
		return module;
	}
	
	public static SystemMenuBuilder getInstance(){
		if(INSTANCE==null)
			INSTANCE = new SystemMenuBuilder();
		return INSTANCE;
	}

	
}
