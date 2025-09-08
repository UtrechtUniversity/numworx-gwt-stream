package nl.numworx.stream;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JComponent;

import nl.numworx.swingbrowser.api.ConsoleEvent;
import nl.numworx.swingbrowser.api.SwingBrowser;
import nl.numworx.swingbrowser.api.SwingBrowserProvider;
import nl.numworx.swingbrowser.scorm.ConsoleListener;
import nl.numworx.swingbrowser.scorm.SCORM2004APIInterface;

public class HTMLBrowser implements SCORM2004APIInterface, ConsoleListener {
    private static final SwingBrowserProvider BROWSER_PROVIDER = new SwingBrowserProvider();

    private static final Logger LOG = Logger.getLogger(HTMLBrowser.class.getName());
    private SwingBrowser browser;
    private volatile boolean inited;
    

    public HTMLBrowser() {
    	browser = BROWSER_PROVIDER.getFactory().newBrowser();
    	browser.setAPI(this);
    	browser.addConsoleListener(this);
    }
    
	public JComponent getBrowserPanel() {
		return browser.asComponent();
	}

	public void loadURL(final String url) {
		  browser.loadURL(url);
		}

	@Override
	public synchronized String Initialize(String dummy) {
		inited = true;
		return "true";
	}

	@Override
	public String Commit(String dummy) {
		return "true";
	}

	@Override
	public synchronized String Terminate(String dummy) {
		inited = false;
		notifyAll();
		return "true";
	}
	
	public synchronized void waitTerminate() {
		if(inited) {
			try {
				wait(1000);
			} catch (InterruptedException e) {
			}
		}
	}
	
	@Override
	public String GetValue(String key) {
		if (StreamInteractiePanel.FLOW.equals(key))
				return flow;
		return "";
	}

	@Override
	public String SetValue(String key, String value) {
		if (StreamInteractiePanel.FLOW.equals(key))
			flow = value;
		return "true";
	}

	@Override
	public String GetLastError() {
		return "0";
	}

	@Override
	public String GetDiagnostic(String iErrorCode) {
		return "No Error";
	}

	@Override
	public String GetErrorString(String iErrorCode) {
		return "No Error";
	}

	private String flow;
	public void setFlow(String flow) {
		this.flow = flow;
	}
	public String getFlow() {
		return flow;
	}

	public void destroy() {
		try {
			browser.close();
		} catch (IOException e) {
			LOG.log(Level.SEVERE, "destroy browser", e);
		}
		browser = null;
		
	}

	@Override
	public void onConsole(ConsoleEvent event) {
		if (event.getLevel() == ConsoleEvent.Level.ERROR)
			System.err.println(event.getMessage());
		else
			System.out.println(event.getMessage());
	}

    
}
