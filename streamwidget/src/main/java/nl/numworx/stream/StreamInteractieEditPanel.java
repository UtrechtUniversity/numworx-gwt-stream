package nl.numworx.stream;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.cbook.cbookif.CBookWidgetEditIF;

import fi.beans.numworxlf.JButton;
import fi.beans.numworxlf.JFormattedTextField;
import fi.beans.numworxlf.JLabel;
import fi.beans.numworxlf.JScrollPane;
import fi.beans.wiskopdrbeans.InteractieEditPanel;

@SuppressWarnings({ "rawtypes", "serial" })
public class StreamInteractieEditPanel extends JPanel implements InteractieEditPanel, CBookWidgetEditIF {

	private static final String SCORE_MAX = "scoreMax";
	int instanceWidth = 600, instanceHeight = 400;
	private Map<String, ?> launchData  = Collections.emptyMap();
	private final Stream parent;
	HTMLBrowser browser;
	JScrollPane scroll;
	JPanel east;
	JButton open, save;
	JFormattedTextField maxScore;
	
	StreamInteractieEditPanel(Stream stream) {
		super(new BorderLayout());
		parent = stream;
		browser = new HTMLBrowser();
		
		scroll = new JScrollPane();
		add(scroll, BorderLayout.CENTER);
		scroll.setPreferredSize(new Dimension(600,400));
		east = new JPanel();
		east.setPreferredSize(new Dimension(200, 400));
		open = new JButton("Import");
		save = new JButton("Export");
		maxScore = new JFormattedTextField();
		maxScore.setValue(0);
		east.add(new JLabel("max score"));
		east.add(maxScore);
		east.add(open);
		east.add(save);
		add(east, BorderLayout.EAST);
		scroll.setViewportView(browser.getBrowserPanel());
	}

	@Override
	public Hashtable getEditState() {
		Hashtable state = new Hashtable();
		String flow = browser.getFlow();
		if (flow != null) state.put(StreamInteractiePanel.FLOW, flow);
		state.put(SCORE_MAX, getMaxScore());
		state.put("checkDocent", getMaxScore()>0);
		launchData = state;
		return state;
	}

	@Override
	public void setEditState(Hashtable h) {
		setLaunchData(h);
	}

	@Override
	public void start() {
		String flow = Objects.toString(launchData.get(StreamInteractiePanel.FLOW), parent.getDefaultFlow());
		browser.setFlow(flow);
		browser.loadURL(parent.getBase().resolve("StreamWidget.html").toString());
	}

	@Override
	public void stop() {
		browser.loadURL(null);

	}

	@Override
	public void zetBreedte(int w) {
		setInstanceWidth(w);
	}

	@Override
	public void zetHoogte(int h) {
		setInstanceHeight(h);
	}

	@Override
	public JComponent asComponent() {
		return this;
	}

	@Override
	public String[] getAcceptedCmds() {
		return null;
	}

	@Override
	public Dimension getInstanceSize() {
		return new Dimension(instanceWidth,instanceHeight);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Map<String, ?> getLaunchData() {		
		return getEditState();
	}

	@Override
	public String getLocalizedCmd(String arg0) {
		return null;
	}

	@Override
	public int getMaxScore() {
		return ((Number)maxScore.getValue()).intValue();
	}

	@Override
	public String[] getSendCmds() {
		return null;
	}

	@Override
	public void setInstanceHeight(int h) {
		instanceHeight = h;
		browser.getBrowserPanel().setSize(getInstanceSize());
	}

	@Override
	public void setInstanceWidth(int w) {
		instanceWidth = w;	
		browser.getBrowserPanel().setSize(getInstanceSize());
	}

	@Override
	public void setLaunchData(Map<String, ?> map) {
		if (map == null) map = new Hashtable<String, Object>();
		launchData = map;
		if (map.containsKey(SCORE_MAX)) {
			maxScore.setValue(map.get(SCORE_MAX));
		} else {
			maxScore.setValue(0);
		}
 	}

}
