package nl.numworx.streamgwt.client;

import java.util.Collections;
import java.util.HashMap;

public class StreamGWTDebug extends StreamGWT {

	public StreamGWTDebug() {
		super();
	}

	@Override
	public void onModuleLoad() {
		String flow = "{}"; // tbs
		HashMap<String, Object> launch = new HashMap<>();
		launch.put(FLOW, flow);
		init(100,100, launch, Collections.emptyMap());
		setState(launch);
	
	}

}
