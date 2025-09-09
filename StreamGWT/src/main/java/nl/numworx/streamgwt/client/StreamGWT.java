package nl.numworx.streamgwt.client;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.InlineHTML;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

import nl.uu.fi.dwo.interaction.client.InteractionStub;
import nl.uu.fi.dwo.interaction.client.JSONUtilities;
import nl.uu.fi.dwo.interaction.client.OpdrNavIF;
import nl.uu.fi.dwo.interaction.client.Stub;
import nl.uu.fi.dwo.interaction.client.json.ObjectMap;

public class StreamGWT extends Composite implements EntryPoint, InteractionStub {

	protected static final String FLOW = "flowchart";
	
	private native static void install(String flow, Consume checkpoint)
	/*-{		
	 	$wnd.install( flow, function(data) { checkpoint.@nl.numworx.streamgwt.client.Consume::accept(Ljava/lang/String;)(data) } )
	 }-*/
	;
	
	String flow = "";
	int width;
	int height;
	
	public StreamGWT() {
		InlineHTML html = new InlineHTML("<!-- Stream GWT -->");
		initWidget(html);
	}

	@Override
	public void onModuleLoad() {
		RootPanel.get().add(this);
		Stub.publish(this);
	}

	@Override
	public HashMap<String, Object> getState() {
		HashMap<String,Object> result = new HashMap<>();
		result.put(FLOW, flow);
		return result;
	}

	@Override
	public void setState(HashMap<String, Object> h) {
		if (h != null) {
			ObjectMap map = JSONUtilities.wrapMap(h);
			if (map.containsKey(FLOW)) {
				flow = map.getString(FLOW);
			}
		}
		install(flow, (data) -> { flow = data; });
	}

	@Override
	public int getScore() {
		return 0;
	}

	@Override
	public int[][] getScoreObjectives() {
		return null;
	}

	@Override
	public Boolean isCorrect() {
		return null;
	}

	@Override
	public void kijkNa() {
	}

	@Override
	public void zetNagekeken(boolean b) {
	}

	@Override
	public void setCommunicationRoot(OpdrNavIF comRoot) {
	}

	@Override
	public void zetVolledigeBreedte(int breedte) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Widget asWidget() {
		return this;
	}

	@Override
	public int getAsHoogte() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getHeight() {
		return height;
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public void setAsHoogte(int ashoogte) {
	}

	@Override
	public void init(int width, int height, Map<String, Object> launchData, Map<String, Number> values) {
		this.width = width;
		this.height = height;
		ObjectMap map = JSONUtilities.wrapMap(launchData);
		if (map.containsKey(FLOW))
			flow = map.getString(FLOW);
	}

}
