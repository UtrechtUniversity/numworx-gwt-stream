package nl.numworx.streamgwt.client;

import java.util.HashMap;
import java.util.Map;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.dom.client.Style;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.InlineHTML;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

import nl.uu.fi.dwo.interaction.client.InteractionStub;
import nl.uu.fi.dwo.interaction.client.JSONUtilities;
import nl.uu.fi.dwo.interaction.client.OpdrNavIF;
import nl.uu.fi.dwo.interaction.client.Stub;
import nl.uu.fi.dwo.interaction.client.event.CBookEvent;
import nl.uu.fi.dwo.interaction.client.json.ObjectMap;

public class StreamGWT extends Composite implements EntryPoint, InteractionStub, ResizeHandler {

	protected static final String FLOW = "flowchart";
	
	private native static void install(String flow, Consume checkpoint)
	/*-{		
	 	$wnd.install( flow, function(data) { checkpoint.@nl.numworx.streamgwt.client.Consume::accept(Ljava/lang/String;)(data) } )
	 }-*/
	;
	
	String flow = "";
	int width;
	int height;
	private int width0;
	private int height0;
	private boolean pastHoogteAan, hasWidth;
	private int heightSVG;
	private OpdrNavIF comRoot;
	
	public StreamGWT() {
		InlineHTML html = new InlineHTML("<!-- Stream GWT -->");
		initWidget(html);
	}

	@Override
	public void onModuleLoad() {
		RootPanel.get().add(this);
		Window.addResizeHandler(this);
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
		install(flow, (data) -> { 
			flow = data;
			if (pastHoogteAan) pasAanH(data);
		});
		if (!hasWidth) {
			width0 = svgWidth().intValue();
		}
		double zoom = width / (double) width0;
		RootPanel.get("outer").getElement().getStyle().setProperty("zoom", Double.toString(zoom));

		
		if (pastHoogteAan) 
			pasAanH(flow);
		else {
			
		}
	}

	private Number svgWidth() {
		Element elem = RootPanel.get("outer").getElement();
		NodeList<Element> list = elem.getElementsByTagName("svg");
		if (list.getLength() > 0) {
			elem = list.getItem(0);
			try {
				String w = width(elem);
				return Double.valueOf(w);
			} catch(Exception oops) { }
		}
		return width0;
	}
	
	
	private void pasAanH(String data) {
		Element elem = RootPanel.get("outer").getElement();
		NodeList<Element> list = elem.getElementsByTagName("svg");
		if (list.getLength() > 0) {
			elem = list.getItem(0);
			try {
				String h = height(elem);
				heightSVG = Double.valueOf(h).intValue()+1;
			} catch(Exception oops) {
			}
		}
		int zoomed = (heightSVG * width + width0-1) / width0;
		if (zoomed > height0 && zoomed != height) {
			HashMap h = new HashMap();
			h.put("height", zoomed);			// houd rekening met zoom.
			comRoot.fireEvent(new CBookEvent(this, "resize", h));
		}
	}

	private native static String height(Element elem) /*-{
		return elem.height.baseVal.valueAsString;
	}-*/;

	private native static String width(Element elem) /*-{
	return elem.width.baseVal.valueAsString;
	}-*/;

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
		this.comRoot = comRoot;
	}

	@Override
	public void zetVolledigeBreedte(int breedte) {
		GWT.log("zet vol breedte " + breedte);
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
		this.width0 = this.width = width;
		this.heightSVG = this.height0 = this.height = height;
		ObjectMap map = JSONUtilities.wrapMap(launchData);
		if (map.containsKey(FLOW))
			flow = map.getString(FLOW);
		boolean readonly = map.getBoolean("readonly", false);
		RootPanel outer = RootPanel.get("outer");
		outer.setStyleName("readonly", readonly);
		pastHoogteAan = map.getBoolean("pasAanH", false);
		boolean notitle = map.getBoolean("noTitle", false);
		outer.setStyleName("noname", notitle);
		hasWidth = map.getBoolean("hasWidth", false);
		if (hasWidth) {
			width0 = map.getInt("width");
		}
		
	}

	@Override
	public void onResize(ResizeEvent event) {
		int w = event.getWidth();
		int h = event.getHeight();
		if (h == 0 || w == 0) return; // false alarm
		if (h == height && w == width) return; // no change
		// pas aan h:
		int h1 = height0 * w / width0;
		GWT.log("h1 = " + h1 + " h= " + h);
		double zoom = w / (double) width0;
		RootPanel.get("outer").getElement().getStyle().setProperty("zoom", Double.toString(zoom));
		width = w;
		height = h;
		
	}

}
