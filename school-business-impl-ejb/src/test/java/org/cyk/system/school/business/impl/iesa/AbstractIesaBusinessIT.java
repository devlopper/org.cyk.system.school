package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.markuplanguage.MarkupLanguageBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.file.report.jasper.JasperReportBusinessImpl;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.utility.common.Constant;

import net.sf.jasperreports.engine.design.JasperDesign;

public abstract class AbstractIesaBusinessIT extends AbstractBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer; 
    @Inject protected MarkupLanguageBusiness markupLanguageBusiness;
     
    protected void installApplication(Boolean fake){
    	
    	super.installApplication(fake);
    	AbstractRootReportProducer.DEFAULT = new IesaFakedDataProducer.ReportProducer();
    	Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	
    	StudentBusinessImpl.Listener.Adapter listener = new StudentBusinessImpl.Listener.Adapter.Default(){
			private static final long serialVersionUID = 1L;
    		@Override
    		public void afterInstanciateOne(UserAccount userAccount, Student student) {
    			super.afterInstanciateOne(userAccount, student);
    			student.setStudentClassroomSession(new StudentClassroomSession(student, null));
    		}
    		
    		@Override
			public void beforeCreate(Student student) {
				super.beforeCreate(student);
				if(StringUtils.isBlank(student.getCode())){
					NumberBusiness.FormatArguments orderNumberFormatArguments = new FormatArguments();
					orderNumberFormatArguments.setWidth(4);
					student.setCode("IESA"+Constant.CHARACTER_SLASH+inject(TimeBusiness.class).findYear(inject(AcademicSessionBusiness.class).findCurrent(null).getBirthDate())
							+inject(PersonBusiness.class).findInitials(student.getPerson())+inject(NumberBusiness.class).format(inject(StudentDao.class).countAll()+1,orderNumberFormatArguments)
							+Constant.CHARACTER_HYPHEN+student.getAdmissionLevelTimeDivision().getLevel().getGroup().getCode()
							);
				}
			}
    	};
    	listener.addCascadeToClass(StudentClassroomSession.class);
    	StudentBusinessImpl.Listener.COLLECTION.add(listener);
    	
    	JasperReportBusinessImpl.Listener.COLLECTION.add(new JasperReportBusinessImpl.Listener.Adapter.Default(){
    		
    		@Override
    		public Boolean isJrxmlProcessable(ReportBasedOnTemplateFile<?> aReport) {
    			Object object = aReport.getDataSource().iterator().next();
    			if(object instanceof StudentClassroomSessionDivisionReportTemplateFile){
    				StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivisionReport = (StudentClassroomSessionDivisionReportTemplateFile) object;
    				return !Boolean.TRUE.equals(((StudentClassroomSessionDivision)studentClassroomSessionDivisionReport.getSource()).getClassroomSessionDivision().getStudentRankable());
    			}
    			return super.isJrxmlProcessable(aReport);
    		}
    		
    		@Override
    		public String processJrxml(ReportBasedOnTemplateFile<?> aReport,String jrxml) {
    			//jrxml = updateTableColumn(jrxml,new Object[]{DETAIL,0,BAND,2,FRAME,0,COMPONENT_ELEMENT,0}, 0, 11, new String[]{WIDTH,"124"});
    			//jrxml = updateTableColumn(jrxml,new Object[]{DETAIL,0,BAND,2,FRAME,0,COMPONENT_ELEMENT,0}, 0, 12, new String[]{WIDTH,"150"});
    			return jrxml;
    		}
    		
			private static final long serialVersionUID = -4233974280078518157L;
    		@Override
    		public void processDesign(ReportBasedOnTemplateFile<?> aReport,JasperDesign jasperDesign) {
    			super.processDesign(aReport,jasperDesign);
    			Object object = aReport.getDataSource().iterator().next();
    			if(object instanceof StudentClassroomSessionDivisionReportTemplateFile){
    				StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivisionReport = (StudentClassroomSessionDivisionReportTemplateFile) object;
    				
    				//((JRDesignExpression)jasperDesign.getParametersMap().get(SchoolConstant.REPORT_CYK_GLOBAL_RANKABLE).getDefaultValueExpression())
    				//	.setText(((StudentClassroomSessionDivision)studentClassroomSessionDivisionReport.getSource()).getClassroomSessionDivision().getStudentSubjectRankable().toString());
    				
    				if(Boolean.TRUE.equals(((StudentClassroomSessionDivision)studentClassroomSessionDivisionReport.getSource()).getClassroomSessionDivision().getStudentRankable())){
    					
    				}else{
    					
    				}
    				
    				/* Color */
    				/*if( classroomSession.getLevelTimeDivision().getLevel().getName().getCode().equals("Grade2") ){
    					jasperDesign.getStylesMap().get("title").setBackcolor(Color.ORANGE);
        				jasperDesign.getStylesMap().get("block header").setBackcolor(Color.ORANGE);
        				((JRBaseLineBox)jasperDesign.getStylesMap().get("block header").getLineBox()).getTopPen().setLineColor(Color.GREEN);
        				((JRBaseLineBox)jasperDesign.getStylesMap().get("block header").getLineBox()).getBottomPen().setLineColor(Color.GREEN);
    				}*/
    			}
    		}
    	});
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
