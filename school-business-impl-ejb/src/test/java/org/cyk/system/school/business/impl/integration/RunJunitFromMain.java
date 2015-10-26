package org.cyk.system.school.business.impl.integration;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

public class RunJunitFromMain {

	public static void main(String[] args) {
		System.out.println("Actual resources file    : "+ApplicationSetupBusinessIT.readArquillianResourceXml());
		ApplicationSetupBusinessIT.updateArquillianResourceXml("src/test/resources-glassfish-embedded/glassfish-resources-live.xml");
    	System.out.println("Updated resources file 1 : "+ApplicationSetupBusinessIT.readArquillianResourceXml());
		System.out.println("RunJunitFromMain.main()");
		Class<?> test = ApplicationSetupBusinessIT.class;
		JUnitCore junit = new JUnitCore();
		Result result = junit.run(test);
		System.out.println("RunJunitFromMain.main() : "+result);
		System.out.println("RunJunitFromMain.main()");
	}

}
