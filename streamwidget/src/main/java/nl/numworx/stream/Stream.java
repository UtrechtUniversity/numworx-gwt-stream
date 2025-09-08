package nl.numworx.stream;

import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.Locale;

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
	}
	
	private URI base = URI.create("http://localhost:8082/apps/"); // ergens uit een context halen.
	private String flow = "";
	
	URI getBase() {
		return base;
	}
	String getDefaultFlow() {
		return flow;
	}

	@Override
	public InteractiePanel getInteractiePanel() {
		return new StreamInteractiePanel(this);
	}

	@Override
	public CBookWidgetEditIF getEditor(CBookContext context) {
		return new StreamInteractieEditPanel(this);
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
		return new StreamInteractiePanel(this);
	}

	public String toString() {
		return "Stream Widget";
	}
}
