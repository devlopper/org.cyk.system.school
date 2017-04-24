package org.cyk.system.school.business.impl.integration;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;

import org.cyk.system.root.business.api.RemoteConnectivityChecker;

public class RemoteConnectivityCheckerClient {
	public static void main(String[] args) {
		System.out.println("client started...");
		try {
			Context context = new InitialContext();
			Object homeObject = context.lookup("java:global/school-ui-web-primefaces-app/RemoteConnectivityCheckerImpl");
			RemoteConnectivityChecker remoteConnectivityChecker = (RemoteConnectivityChecker) PortableRemoteObject.narrow(homeObject, RemoteConnectivityChecker.class);
			remoteConnectivityChecker.echo("Hello! I ping the server");
			System.out.println("Date from server : "+ remoteConnectivityChecker.getDate());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}