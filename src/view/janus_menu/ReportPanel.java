package view.janus_menu;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JTextArea;

public class ReportPanel extends JPanel{
	private static final long serialVersionUID = 1L;
	
	private JTextArea textArea = new JTextArea();
	
	public ReportPanel(){
		super();
		this.setLayout(new BorderLayout());
		this.add(textArea, BorderLayout.CENTER);
		//TODO add scrollbars and clear button etc
	}

}
