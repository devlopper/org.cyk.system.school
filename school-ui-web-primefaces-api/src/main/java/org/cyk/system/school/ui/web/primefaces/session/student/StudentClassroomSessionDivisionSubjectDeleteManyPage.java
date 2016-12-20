package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionSubjectDeleteManyPage extends AbstractCrudOnePage<StudentClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<StudentSubjectItem,StudentClassroomSessionDivisionSubject> collection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		collection = createItemCollection(StudentSubjectItem.class, StudentClassroomSessionDivisionSubject.class 
				,new ItemCollectionWebAdapter<StudentSubjectItem,StudentClassroomSessionDivisionSubject>(){
			private static final long serialVersionUID = -3872058204105902514L;
			
			@Override
			public Collection<StudentClassroomSessionDivisionSubject> create() {
				System.out.println(inject(StudentClassroomSessionDivisionSubjectDao.class).countAll());
				Collection<StudentClassroomSessionDivisionSubject> collection = inject(StudentClassroomSessionDivisionSubjectDao.class).readDuplicates();
				System.out.println(collection.size());
				//for(StudentClassroomSessionDivisionSubject i : collection)
				//	System.out.println(i);
				Collection<StudentClassroomSessionDivisionSubject> collection2 = new ArrayList<>();
				for(StudentClassroomSessionDivisionSubject i : collection)
					for(StudentClassroomSessionDivisionSubject k : inject(StudentClassroomSessionDivisionSubjectDao.class).readDuplicatesByStudentByClassroomSessionDivisionBySubject(
							i.getStudent(), i.getClassroomSessionDivisionSubject().getClassroomSessionDivision(), i.getClassroomSessionDivisionSubject().getSubject()))
					collection2.add(k);
				for(StudentClassroomSessionDivisionSubject s : collection2){
					s.getDetailCollection().getCollection().clear();
					s.getDetailCollection().getCollection().addAll(inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).find(s));
				}
				return collection2;
			}
			
			@Override
			public Crud getCrud() {
				return crud;
			}
			
			@Override
			public void setLabel(AbstractItemCollection<StudentSubjectItem, StudentClassroomSessionDivisionSubject, SelectItem> itemCollection,StudentSubjectItem item) {
				super.setLabel(itemCollection, item);
				Collection<String> s = new ArrayList<>();
				for(StudentClassroomSessionDivisionSubjectEvaluation n : item.getIdentifiable().getDetailCollection().getCollection())
					s.add(inject(NumberBusiness.class).format(n.getValue()));
				item.setLabel(item.getIdentifiable().getStudent().getCode()+" | "+item.getIdentifiable().getClassroomSessionDivisionSubject()+" | "
						+StringUtils.join(s," | "));
			}
			
			@Override
			public void instanciated(AbstractItemCollection<StudentSubjectItem, StudentClassroomSessionDivisionSubject, SelectItem> itemCollection,StudentSubjectItem item) {
				super.instanciated(itemCollection, item);
				item.setApplicable(Boolean.FALSE);
			}
		});
		collection.setShowItemLabel(Boolean.TRUE);
		collection.getApplicableValueQuestion().setRendered(Boolean.TRUE);
	}
		
	@Override
	protected void create() {
		inject(GenericBusiness.class).delete(commonUtils.castCollection(collection.getIdentifiables(),AbstractIdentifiable.class));
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected Object identifiableFormData(Class<?> dataClass) throws InstantiationException, IllegalAccessException {
		return new Object();
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(StudentClassroomSessionDivisionSubject.class);
	}
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.CREATE;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<StudentClassroomSessionDivisionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
	}
	
	@Getter @Setter
	public static class StudentSubjectItem extends AbstractItemCollectionItem<StudentClassroomSessionDivisionSubject> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		
	}
	
}
