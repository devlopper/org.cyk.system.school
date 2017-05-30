package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.party.person.ActorReport;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionReport extends AbstractIdentifiableReport<ClassroomSessionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private AcademicSessionReport academicSession = new AcademicSessionReport();
	@Deprecated private String numberOfStudents,average,highestAverage,lowestAverage;
	private ActorReport coordinator = new ActorReport();
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			academicSession.setSource( ((ClassroomSession)source).getAcademicSession() );
			coordinator.setSource(((ClassroomSession)source).getCoordinator());
		}
	}
	
	@Override
	public void generate() {
		globalIdentifier.setName((String) provider.randomFromList(RANDOM_NAMES));
		numberOfStudents = provider.randomPositiveInt(20)+"";
		average = provider.randomPositiveInt(20)+"";
		highestAverage = provider.randomPositiveInt(20)+"";
		lowestAverage = provider.randomPositiveInt(20)+"";
		
		academicSession.getCompany().getGlobalIdentifier().setGenerateImage(Boolean.TRUE);
		academicSession.getCompany().setGenerateBackground(Boolean.TRUE);
		academicSession.generate();
		
		coordinator.getPerson().setGenerateSignatureSpecimen(Boolean.TRUE);
		coordinator.generate();
	}
	
	/**/
	
	public static final List<String> RANDOM_NAMES = new ArrayList<>();
	static{
		for(int i=1;i<=12;i++)
			RANDOM_NAMES.add("G"+i);
	}
	
}
