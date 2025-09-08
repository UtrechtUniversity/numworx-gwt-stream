package nl.numworx.stream;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.cbook.cbookif.AssessmentMode;
import org.cbook.cbookif.CBookEvent;
import org.cbook.cbookif.CBookEventHandler;
import org.cbook.cbookif.CBookEventListener;
import org.cbook.cbookif.CBookWidgetInstanceIF;
import org.cbook.cbookif.SuccessStatus;

import fi.beans.wiskopdrbeans.InteractieEditPanel;
import fi.beans.wiskopdrbeans.InteractiePanel;

@SuppressWarnings({ "rawtypes", "serial" })
public class StreamInteractiePanel extends JPanel implements InteractiePanel, CBookWidgetInstanceIF, CBookEventListener {

	public static final String FLOW = "flowchart";
	private static final Logger LOG = Logger.getLogger(StreamInteractiePanel.class.getName());
	private final CBookEventHandler handler = new CBookEventHandler(this);
	
	private final Stream parent;
	private final HTMLBrowser browser;
	
	
	StreamInteractiePanel(Stream stream) {
		super(new BorderLayout());
		parent = stream;
		browser = new HTMLBrowser();
		add(browser.getBrowserPanel(), BorderLayout.CENTER);
	}

	@Override
	public void addActionListener(ActionListener listener) {
	}

	@Override
	public void destroy() {
		browser.destroy();
	}

	@Override
	public InteractieEditPanel getEditPanel() {
		return new StreamInteractieEditPanel(parent);
	}

	@Override
	public Hashtable getEditState() {
		Hashtable state = new Hashtable();
		return state;
	}

	@Override
	public int getIpId() {
		return 0;
	}

	@Override
	public int getScore() {
		return 0;
	}

	@Override
	public int getScoreMax() {
		return 0;
	}

	@Override
	public int[][] getScoreObjectives() {
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Hashtable getState() {
		Hashtable state = new Hashtable();
		return state;
	}

	@Override
	public boolean isCorrect() {
		return true;
	}

	@Override
	public boolean isFout() {
		return false;
	}

	@Override
	public void kijkNa() {
	}

	@Override
	public void kijkNa(int arg0) {
	}

	@Override
	public void opnieuw() {
	}


	@SuppressWarnings("unchecked")
	@Override
	public void setEditState(Hashtable map) {
		setLaunchData(map, null);		
	}

	@Override
	public void setState(Hashtable arg0) {
	}

	@Override
	public void start() {
		LOG.info("start");
	}

	@Override
	public void stop() {
		browser.loadURL(null);
	}

	@Override
	public void wis() {
	}

	@Override
	public void zetMaat() {
	}

	@Override
	public void zetMode(int arg0) {
	}

	@Override
	public void zetNagekeken(boolean arg0) {
	}

	@Override
	public void zetOpdracht(Hashtable arg0, String[] arg1, Hashtable arg2) {
	  setLaunchData(arg0, arg2);
	}

	@Override
	public void addCBookEventListener(CBookEventListener listener, String cmd) {
		handler.addCBookEventListener(listener, cmd);		
	}

	@Override
	public JComponent asComponent() {
		return this;
	}

	@Override
	public CBookEventListener asEventListener() {
		return this;
	}

	@Override
	public SuccessStatus getSuccessStatus() {
		return SuccessStatus.PASSED;
	}

	@Override
	public void init() {
		browser.loadURL(null);
	}

	@Override
	public void removeCBookEventListener(CBookEventListener listener, String cmd) {
		handler.removeCBookEventListener(listener, cmd);	
	}

	@Override
	public void reset() {
	}

	@Override
	public void setAssessmentMode(AssessmentMode arg0) {
	}

	@Override
	public void setLaunchData(Map<String, ?> launchdata, Map<String, Number> random) {
		browser.loadURL(null);
		browser.waitTerminate();
		String flow = Objects.toString(launchdata.get(FLOW), parent.getDefaultFlow());
		browser.setFlow(flow);
		browser.loadURL(parent.getBase().resolve("StreamWidget.html").toString());
		
	}

	@Override
	public void setState(Map<String, ?> arg0) {		
	}

	@Override
	public void acceptCBookEvent(CBookEvent arg0) {
	}

}
