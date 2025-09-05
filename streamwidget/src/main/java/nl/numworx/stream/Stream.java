package nl.numworx.stream;

import java.util.Locale;

import fi.beans.mainframe.JApplet;
import fi.beans.wiskopdrbeans.InteractiePanel;
import fi.beans.wiskopdrbeans.WiskOpdrApplet;

@SuppressWarnings("serial")
public class Stream extends JApplet implements WiskOpdrApplet {

	public Stream(Locale locale) {
		setLocale(locale);
	}

	@Override
	public InteractiePanel getInteractiePanel() {
		return new StreamInteractiePanel();
	}

}
