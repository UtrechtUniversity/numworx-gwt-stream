package nl.numworx.stream;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import org.cbook.cbookif.CBookContext;
import org.cbook.cbookif.CBookEvent;
import org.cbook.cbookif.CBookEventListener;
import org.cbook.cbookif.CBookService;
import org.cbook.cbookif.CBookWidgetIF;
import org.cbook.cbookif.Constants;
import org.cbook.cbookif.ServiceImpl;
import org.cbook.cbookif.rm.ResourceManager;
import org.json.simple.parser.ParseException;

import cbookeditor.CBookEditor;
import fi.wiskopdr.WiskOpdr;

public class Main {

	private static CBookEditor editor;
	private static AppContext  appcontext;

	public static void main(String[] args) throws Exception {
		Locale.setDefault(new Locale("nl")); // define your locale here 
		
// Alleen met cbookeditor
		WiskOpdr.applet = new WiskOpdr();
		WiskOpdr.dwo_env = "test";
		//FormuleParser.zetWoordFormule(true);

		appcontext = new AppContext();
		final CBookService service = new ServiceImpl();

// register here your widgets
// by code		
		CBookWidgetIF widget;// = new org.cbook.mediaman.MediaMan();
		//service.registerWidget(widget);
		widget = new nl.numworx.stream.Stream(Locale.getDefault());
		service.registerWidget(widget);
		SwingUtilities.invokeLater(
		new Runnable() {
			public void run() {
				JFrame player;
				appcontext.load();
				player = createEditor(service, appcontext);
				player.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				player.show();
			}
		});
	}
	
	private static JFrame createEditor(CBookService service, CBookContext context) {
		final JLabel logger = new JLabel("");
		editor = new CBookEditor(logger, service, new CBookEventListener() {
			
			public void acceptCBookEvent(CBookEvent event) {
				System.out.print(event.getSource().getClass().getName());
				System.out.print(": ");
				System.out.print(event.getCommand());
				System.out.print(", ");
				System.out.print(event.getMessage());
				System.out.print(", ");
				System.out.println(event.getParameters());
				String tekst = editor.getSuccessStatus() + " " + editor.getScore() + "/" + editor.getMaxScore();
				logger.setText(tekst);
			}
		}, context);
		JFrame frame = new JFrame("CBookEditor");
		frame.setContentPane(editor);
		frame.setSize(editor.getSize());
		frame.addWindowListener(new EditorClosing());
		frame.pack();
		try {
			FileReader reader = new FileReader("cmi.launch_data");
			editor.readLaunchData(reader);
			reader.close();			
			reader = new FileReader("cmi.suspend_data");
			editor.readSuspendData(reader);
			reader.close();
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}
		
		return frame;
	}

	static class EditorClosing extends WindowAdapter {

		@Override
		public void windowClosing(WindowEvent arg0) {
			try {
				FileWriter writer = new FileWriter("cmi.launch_data");
				editor.writeLaunchData(writer);
				writer.close();
				
				writer = new FileWriter("cmi.suspend_data");
				editor.writeSuspendData(writer);
				writer.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
	}
	
	static class AppContext implements CBookContext, Constants {

		String learner_id = "";
		ResourceManager rm;
		
		public Object getProperty(String key) {
			if(LEARNER_ID .equals( key ))
				return learner_id;
			if(Constants.RESOURCE_MANAGER.equals(key))
				return rm;
			return null;
		}
		
		void load() {
			String line = null;
			try {
				FileReader input = new FileReader("cmi.learner_id");
				BufferedReader br = new BufferedReader(input);
				line = br.readLine();
				br.close();
			} catch (Exception __) {}
			if(line != null)
				learner_id = line.trim();
			line = null; // line = JOptionPane.showInputDialog("Username", learner_id);
			if(line != null) 
			{	learner_id = line.trim();
				try {
					FileWriter output = new FileWriter("cmi.learner_id");
					output.write(learner_id);
					output.close();
				} catch(Exception __) {}
			}
		}
		
	}
	

}
