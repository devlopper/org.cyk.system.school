package org.cyk.system.school.business.impl.integration;

import org.cyk.system.school.business.impl.iesa.IesaApplicationSetupBusinessIT;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

public class RunJunitFromMain {

	public static void main(String[] args) {
		System.out.println("Actual resources file    : "+IesaApplicationSetupBusinessIT.readArquillianResourceXml());
		IesaApplicationSetupBusinessIT.updateArquillianResourceXml("src/test/resources-glassfish-embedded/glassfish-resources-live.xml");
    	System.out.println("Updated resources file 1 : "+IesaApplicationSetupBusinessIT.readArquillianResourceXml());
		System.out.println("RunJunitFromMain.main()");
		Class<?> test = IesaApplicationSetupBusinessIT.class;
		JUnitCore junit = new JUnitCore();
		Result result = junit.run(test);
		System.out.println("RunJunitFromMain.main() : "+result);
		System.out.println("RunJunitFromMain.main()");
	}

}
