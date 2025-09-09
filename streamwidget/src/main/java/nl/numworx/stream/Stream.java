package nl.numworx.stream;

import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.cbook.cbookif.CBookContext;
import org.cbook.cbookif.CBookWidgetEditIF;
import org.cbook.cbookif.CBookWidgetIF;
import org.cbook.cbookif.CBookWidgetInstanceIF;

import fi.beans.mainframe.JApplet;
import fi.beans.wiskopdrbeans.InteractiePanel;
import fi.beans.wiskopdrbeans.WiskOpdrApplet;

@SuppressWarnings("serial")
public class Stream extends JApplet implements WiskOpdrApplet, CBookWidgetIF {

	public Stream(Locale locale) {
		setLocale(locale);
		try {
			InputStream in = getClass().getResourceAsStream("resources/flow.json");
			byte[] buf = new byte[in.available()];
			in.read(buf);
			flow = new String(buf, 0); // all ASCII
		} catch (Exception oops) {
			
		}
		rb = ResourceBundle.getBundle("nl.numworx.stream.text.Text", locale);
	}
	
	public Stream() {
		this(Locale.getDefault());
	}
	
	private URI base = URI.create("https://cdn.dwo.nl/apps/"); // ergens uit een context halen.
	private String flow = "";
	private ResourceBundle rb;
	
	URI getBase() {
		return base;
	}
	
	String getString(String key) {
		return rb.getString(key);
	}
	String getDefaultFlow() {
		return flow;
	}

	@Override
	public InteractiePanel getInteractiePanel() {
		String apps = getParameter("appUrlPath");
		if (apps != null) base = URI.create(apps);
		return new StreamInteractiePanel(this);
	}

	@Override
	public CBookWidgetEditIF getEditor(CBookContext context) {
		setBase(context);
		return new StreamInteractieEditPanel(this);
	}

	private void setBase(CBookContext context) {
		base = URI.create("http://localhost:8082/apps/"); // context is testing 
	}

	@Override
	public Icon getIcon() {
		URL u = getClass().getResource("resources/stream.png");
		if (u != null)
			return new ImageIcon(u);
		return null;
	}

	@Override
	public CBookWidgetInstanceIF getInstance(CBookContext context) {
		setBase(context);
		return new StreamInteractiePanel(this);
	}

	public String toString() {
		return "Stream Widget";
	}
}
