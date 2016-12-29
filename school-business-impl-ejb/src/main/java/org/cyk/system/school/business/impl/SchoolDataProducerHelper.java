package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.inject.Singleton;

import org.cyk.system.root.business.impl.RootDataProducerHelper;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.cdi.BeanAdapter;

import lombok.Getter;
import lombok.Setter;

@Singleton
public class SchoolDataProducerHelper extends AbstractBean implements Serializable {

	private static final long serialVersionUID = -8721724629218389127L;

	private static SchoolDataProducerHelper INSTANCE ;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
	}
	
	public CommonNodeInformations instanciateOneCommonNodeInformations(IntervalCollection intervalCollection,IntervalCollection studentClassroomSessionAveragePromotionScale
			,ReportTemplate reportTemplate,String attendanceTimeDivisionTypeCode,String classroomSessionTimeDivisionTypeCode,String evaluationPassAverage
			,String currentClassroomSessionDivisionIndex){
		CommonNodeInformations commonNodeInformations = new CommonNodeInformations(intervalCollection,studentClassroomSessionAveragePromotionScale,reportTemplate
				,RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class,attendanceTimeDivisionTypeCode),new BigDecimal(evaluationPassAverage));
		commonNodeInformations.setClassroomSessionTimeDivisionType(RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class, classroomSessionTimeDivisionTypeCode));
		commonNodeInformations.setCurrentClassroomSessionDivisionIndex(new Long(currentClassroomSessionDivisionIndex));
		commonNodeInformations.setClassroomSessionDivisionOrderNumberInterval(inject(IntervalDao.class).read(SchoolConstant.Code.Interval.DIVISION_COUNT_BY_CLASSROOM_SESSION));
		return commonNodeInformations;
	}
	
	public void addSubjects(Collection<String> subjectCodes,ArrayList<Subject>[] collections){
		if(collections!=null){
			Collection<Subject> subjects = inject(SubjectDao.class).read(subjectCodes);
			for(Collection<Subject> collection : collections)
				collection.addAll(subjects);
		}
	}

	/**/
	
	public static SchoolDataProducerHelper getInstance() {
		return INSTANCE;
	}
	
	/**/
	
	@Getter @Setter
	public static class ClassroomSessionInfos{
		private ClassroomSession classroomSession;
		private List<ClassroomSessionDivisionInfos> divisions = new ArrayList<>(); 
		
		public ClassroomSessionInfos(ClassroomSession classroomSession) {
			super();
			this.classroomSession = classroomSession;
		}
		
		public ClassroomSessionDivisionInfos division(Integer index){
			return divisions.get(index);
		}
		
		public ClassroomSessionDivisionSubject subject(Integer index,Integer subjectIndex){
			return division(index).getSubjects().get(subjectIndex).getClassroomSessionDivisionSubject();
		}
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionInfos{
		private ClassroomSessionDivision classroomSessionDivision; 
		private List<ClassroomSessionDivisionSubjectInfos> subjects = new ArrayList<>();
		
		public ClassroomSessionDivisionInfos(ClassroomSessionDivision classroomSessionDivision) {
			super();
			this.classroomSessionDivision = classroomSessionDivision;
		}
		
		public ClassroomSessionDivisionSubjectInfos subject(Integer index){
			return subjects.get(index);
		}
		
		public List<ClassroomSessionDivisionSubject> getClassroomSessionDivisionSubjects(){
			List<ClassroomSessionDivisionSubject> list = new ArrayList<>();
			for(ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos : subjects)
				list.add(classroomSessionDivisionSubjectInfos.getClassroomSessionDivisionSubject());
			return list;
		}

		public Collection<ClassroomSessionDivisionSubjectEvaluationType> getEvaluationTypes() {
			Collection<ClassroomSessionDivisionSubjectEvaluationType> evaluationTypes = new ArrayList<>();
			for(ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos : subjects)
				evaluationTypes.addAll(classroomSessionDivisionSubjectInfos.evaluationTypes);
			return evaluationTypes;
		}
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionSubjectInfos{
		private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		private List<ClassroomSessionDivisionSubjectEvaluationType> evaluationTypes = new ArrayList<>();
		
		public ClassroomSessionDivisionSubjectInfos(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super();
			this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		}
		
		public ClassroomSessionDivisionSubjectEvaluationType evaluationType(Integer index){
			return evaluationTypes.get(index);
		}
		
	}
	
	/**/
	
	public static interface Listener {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		void classroomSessionDivisionCreated(ClassroomSessionDivision classroomSessionDivision);
		void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);
		
		public static class Adapter extends BeanAdapter implements Listener,Serializable{
			private static final long serialVersionUID = -7938520926769839615L;
			
			/**/
			
			@Override
			public void classroomSessionDivisionCreated(ClassroomSessionDivision classroomSessionDivision) {}
			
			@Override
			public void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {}
			
			public static class Default extends Adapter implements Serializable {
				private static final long serialVersionUID = -5680372873034239621L;
				
			}
		}
	}
}
