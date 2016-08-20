package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.company.business.api.sale.SaleBusiness;
import org.cyk.system.company.model.sale.SaleProduct;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionTuitionEditPage extends AbstractCrudOnePage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		if(identifiable.getTuitionSale()==null){
			identifiable.setTuitionSale(inject(SaleBusiness.class).instanciateOne(null, null,identifiable.getStudent().getCode()
					, null, "false", new String[][]{ {SchoolConstant.INTANGIBLE_PRODUCT_TUITION,"1"} }));
		}
	}
	
	@Override
	public String getTitle() {
		return text("school.definetuition.page.title");
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputNumber @NotNull private BigDecimal amount;
		
		public static final String AMOUNT = "amount";
		
		@Override
		public void write() {
			super.write();
			SaleProduct saleProduct = identifiable.getTuitionSale().getSaleProducts().iterator().next();
			saleProduct.getCost().setValue(amount);
			inject(SaleBusiness.class).applyChange(identifiable.getTuitionSale(), saleProduct);
		}
	}
	
}
