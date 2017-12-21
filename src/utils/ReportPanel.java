package utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class ReportPanel extends JPanel implements ActionListener{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public static void main(String args[]){
		ReportPanel me = new ReportPanel();
		JFrame frame = new JFrame("test");
		frame.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);
					}
				}
		);
		frame.add(me, BorderLayout.CENTER);
		frame.setSize(800, 600);
		JPanel p1 = new JPanel();
		JTextField text1 = new JTextField(20);
		JButton b1 = new JButton("add");
		b1.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				me.append("test");
			}
		});
		p1.add(text1);
		p1.add(b1);
		JPanel p2 = new JPanel();
		p2.add(p1);
		frame.add(p2,BorderLayout.SOUTH);
		frame.setVisible(true);
		frame.validate();
	}
	
	private JTextArea reportText = new JTextArea("",25,40);

	public ReportPanel(){
		this.setLayout(new BorderLayout());
		JPanel controlPanel = new JPanel();
		JPanel reportPanel = new JPanel();
		JButton b1 = new JButton("Clear");
		b1.addActionListener(this);
		controlPanel.add(b1);
		JScrollPane reportScroller = new JScrollPane(reportText);
		reportPanel.add(reportScroller);
		this.add(reportPanel, BorderLayout.CENTER);
		this.add(controlPanel,BorderLayout.SOUTH);
		this.setBorder(BorderFactory.createLineBorder(Color.RED));
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		System.out.println("command " + event.getActionCommand());
		if (event.getActionCommand().compareToIgnoreCase("clear") == 0){
			clear();
		}
	}
	
	public void clear(){
		reportText.setText("");
	}
	
	public void append(String message){
		reportText.append("\n"+ message);
	}
	
	public void setSize(int width, int height){
		reportText.setRows(height);
		reportText.setColumns(width);
	}

}
