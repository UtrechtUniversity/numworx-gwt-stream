package nl.numworx.stream;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.CharBuffer;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import org.cbook.cbookif.CBookWidgetEditIF;

import fi.beans.numworxlf.JButton;
import fi.beans.numworxlf.JFileChooser;
import fi.beans.numworxlf.JFormattedTextField;
import fi.beans.numworxlf.JLabel;
import fi.beans.numworxlf.JScrollPane;
import fi.beans.wiskopdrbeans.InteractieEditPanel;

@SuppressWarnings({ "rawtypes", "serial" })
public class StreamInteractieEditPanel extends JPanel implements InteractieEditPanel, CBookWidgetEditIF, ActionListener {

	private static final String SCORE_MAX = "scoreMax";
	int instanceWidth = 600, instanceHeight = 400;
	private Map<String, ?> launchData  = Collections.emptyMap();
	private final Stream parent;
	HTMLBrowser browser;
	JScrollPane scroll;
	Box east;
	JButton open, save;
	JFormattedTextField maxScore;
	
	StreamInteractieEditPanel(Stream stream) {
		super(new BorderLayout());
		parent = stream;
		browser = new HTMLBrowser();
		
		scroll = new JScrollPane();
		add(scroll, BorderLayout.CENTER);
		scroll.setPreferredSize(new Dimension(600,400));
		east = Box.createVerticalBox();
		east.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 0));
		east.setPreferredSize(new Dimension(200, 400));
		open = new JButton(stream.getString("import"));
		save = new JButton(stream.getString("export"));
		maxScore = new JFormattedTextField();
		maxScore.setColumns(5);
		maxScore.setValue(0);
		maxScore.setMaximumSize(maxScore.getPreferredSize());
		
		JLabel label;
		label = new JLabel(stream.getString("nakijken")); label.setFont(new Font("sansserif", Font.BOLD, 18));
		label.setAlignmentX(0);
		east.add(label);
		east.add(Box.createVerticalStrut(10));
		label = new JLabel(stream.getString("maxScore"));
		Box max = Box.createHorizontalBox();
		max.add(label); max.add(maxScore);
		max.setAlignmentX(0);
		east.add(max);
		east.add(Box.createVerticalStrut(20));
		label = new JLabel(stream.getString("flows")); label.setFont(new Font("sansserif", Font.BOLD, 18));
		label.setAlignmentX(0);
		east.add(label);
		east.add(Box.createVerticalStrut(10));
		open.setAlignmentX(0);
		save.setAlignmentX(0);
		east.add(open);
		east.add(Box.createVerticalStrut(5));
		east.add(save);
		east.add(Box.createVerticalGlue());
		add(east, BorderLayout.EAST);
		scroll.setViewportView(browser.getBrowserPanel());
		label = new JLabel(stream.getString("frank"));
		label.setHorizontalTextPosition(JLabel.LEFT);
		label.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));
		add(label, BorderLayout.SOUTH);
		open.addActionListener(this);
		save.addActionListener(this);
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
		loadStreamWidget();
	}

	private void loadStreamWidget() {
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

	JFileChooser chooser;

	private synchronized JFileChooser getChooser() {
		if (chooser == null) {
			chooser = new JFileChooser();
			FileFilter filter = new FileFilter() {

				@Override
				public boolean accept(File f) {
					return f.getName().endsWith(".json") || f.isDirectory();
				}

				@Override
				public String getDescription() {
					return "JSON Files";
				}
				
			};
			chooser.addChoosableFileFilter(filter);
			
		}
		return chooser;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == save) {
			int result = getChooser().showSaveDialog(this);
			if (result == JFileChooser.APPROVE_OPTION) {
				File output = getChooser().getSelectedFile();
				String json = browser.getFlow();
				try (FileOutputStream out = new FileOutputStream(output)) {
					OutputStreamWriter writer = new OutputStreamWriter(out, "UTF-8");
					writer.write(json);
					writer.close();
				} catch(IOException ioe) {
				}				
			}
			return;
		}
		if (e.getSource() == open) {
			int result = getChooser().showOpenDialog(this);
			if (result == JFileChooser.APPROVE_OPTION) {
				File input = getChooser().getSelectedFile();
				try (FileInputStream in = new FileInputStream(input)) {
					InputStreamReader reader = new InputStreamReader(in, "UTF-8");
					int length = in.available();
					CharBuffer buffer= CharBuffer.allocate(length);
					reader.read(buffer);
					browser.loadURL(null);
					browser.waitTerminate();
					buffer.flip();
					browser.setFlow(buffer.toString());
					loadStreamWidget();
				} catch (IOException ioe) {
					
				}
			}
		}
	}

}
