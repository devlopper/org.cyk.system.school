package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.session.LevelNameBusiness;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.persistence.api.session.LevelNameDao;

public class LevelNameBusinessImpl extends AbstractEnumerationBusinessImpl<LevelName, LevelNameDao> implements LevelNameBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelNameBusinessImpl(LevelNameDao dao) {
		super(dao); 
	}
	
	@Override
	protected LevelName __instanciateOne__(String[] values,InstanciateOneListener<LevelName> listener) {
		super.__instanciateOne__(values, listener);
		set(listener.getSetListener().setIndex(10), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_CLASSROOM_SESSION_DIVISION_ORDER_NUMBER_INTERVAL);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_TEMPLATE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_SIGNER);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_SUBJECT_AVERAGESCALE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_AVERAGE_SCALE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_SCALE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_PROMOTION_SCALE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_ATTENDANCE_TIME_DIVISION_TYPE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_EVALUATION_PASS_AVERAGE);
		set(listener.getSetListener(), LevelName.FIELD_NODE_INFORMATIONS,CommonNodeInformations.FIELD_AGGREGATE_ATTENDANCE);
		return listener.getInstance();
	}

}
