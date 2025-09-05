package nl.numworx.stream;

import java.awt.event.ActionListener;
import java.util.Hashtable;

import javax.swing.JPanel;

import fi.beans.wiskopdrbeans.InteractieEditPanel;
import fi.beans.wiskopdrbeans.InteractiePanel;

@SuppressWarnings({ "rawtypes", "serial" })
public class StreamInteractiePanel extends JPanel implements InteractiePanel {

	@Override
	public void addActionListener(ActionListener arg0) {
	}

	@Override
	public void destroy() {
	}

	@Override
	public InteractieEditPanel getEditPanel() {
		return new StreamInteractieEditPanel();
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

	@Override
	public Hashtable getState() {
		Hashtable state = new Hashtable();
		return state;
	}

	@Override
	public boolean isCorrect() {
		return false;
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


	@Override
	public void setEditState(Hashtable arg0) {
	}

	@Override
	public void setState(Hashtable arg0) {
	}

	@Override
	public void start() {
	}

	@Override
	public void stop() {
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
	}

}
