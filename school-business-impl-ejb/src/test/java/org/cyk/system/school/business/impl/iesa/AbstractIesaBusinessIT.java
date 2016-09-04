package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.api.markuplanguage.MarkupLanguageBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.file.report.jasper.JasperReportBusinessImpl;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.Evaluation;

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
    	};
    	listener.addCascadeToClass(StudentClassroomSession.class);
    	StudentBusinessImpl.Listener.COLLECTION.add(listener);
    	JasperReportBusinessImpl.Listener.COLLECTION.add(new JasperReportBusinessImpl.Listener.Adapter.Default(){
    		
    		@Override
    		public Boolean isJrxmlProcessable(ReportBasedOnTemplateFile<?> aReport) {
    			Object object = aReport.getDataSource().iterator().next();
    			if(object instanceof StudentClassroomSessionDivisionReport){
    				StudentClassroomSessionDivisionReport studentClassroomSessionDivisionReport = (StudentClassroomSessionDivisionReport) object;
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
    			if(object instanceof StudentClassroomSessionDivisionReport){
    				StudentClassroomSessionDivisionReport studentClassroomSessionDivisionReport = (StudentClassroomSessionDivisionReport) object;
    				
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
