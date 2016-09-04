package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.utility.common.ListenerUtils;

import lombok.Getter;
import lombok.Setter;

@Stateless
public class StudentBusinessImpl extends AbstractActorBusinessImpl<Student, StudentDao,SearchCriteria> implements StudentBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentBusinessImpl(StudentDao dao) {
		super(dao);  
	}
	
	@Override
	public Student create(final Student student) {
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.beforeCreate(student);
			}});
		super.create(student);
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.afterCreate(student);
			}});
		return student;
	}
	
	@Override
	public Student update(final Student student) {
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.beforeUpdate(student);
			}});
		super.update(student);
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.afterUpdate(student);
			}});
		return student;
	}
	
	@Override
	public Student delete(final Student student) {
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.beforeDelete(student);
			}});
		super.delete(student);
		listenerUtils.execute(Listener.COLLECTION, new ListenerUtils.VoidMethod<Listener>(){
			@Override
			public void execute(Listener listener) {
				listener.afterDelete(student);
			}});
		return student;
	}

	/**/

	public static interface Listener extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener<Student>{
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/

		public static class Adapter extends org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl.Listener.Adapter<Student> implements Listener, Serializable {
			private static final long serialVersionUID = -1625238619828187690L;
			
			/**/
			@Getter @Setter
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = -1625238619828187690L;
				
				/**/
				
				@Override
				public void afterCreate(Student student) {
					super.afterCreate(student);
					if(containsCascadeToClass(StudentClassroomSession.class) && student.getStudentClassroomSession()!=null && student.getStudentClassroomSession().getIdentifier()==null){
						student.getStudentClassroomSession().setStudent(student);
						inject(StudentClassroomSessionBusiness.class).create(student.getStudentClassroomSession());
					}		
				}
				
				
			}
			
		}
		
	}	
}
