package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;

import utils.Tracer;


public class TraceView extends JPanel implements TraceListener {
	
	private JTextArea traceArea = new JTextArea();

	public static void main(String[] args){
		TraceView view = new TraceView();
		JFrame frame = view.makeTestFrame();
		
		frame.add(view,BorderLayout.CENTER);
		frame.add(view.makeTestControler(),BorderLayout.NORTH);
		
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		
		Tracer.addListener(view);
		
		Tracer.write("test");

	}
	
	private JPanel makeTestControler(){
		JPanel panel = new JPanel();
		JButton testBtn = new JButton("test");
		panel.add(testBtn);
		testBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				Tracer.write("another test");
			}
		});
		return panel;
	}
	
	private JFrame makeTestFrame(){
		JFrame frame = new JFrame("trace");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
//		frame.setLocationRelativeTo(null);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		int height = screenSize.height;
		int width = screenSize.width;
		frame.setPreferredSize(new Dimension(width, height));
		frame.setMinimumSize(new Dimension(100,100));
		// seems to do nothing frame.setSize(width, height);
		// shifts so top left is centre screen - dumb frame.setLocationRelativeTo(null);
		// does nothing = should make full screenframe.setState(Frame.NORMAL);
		return frame;
	}
	
	public TraceView(){
		super();
		traceArea = new JTextArea();
		traceArea.setFont(new Font("Arial", Font.PLAIN, 10));
		this.setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(traceArea);
		this.setBackground(Color.green);
		//pane.setPreferredSize(new Dimension(300,200));
		pane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		pane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		
		this.add(pane,BorderLayout.CENTER);

		JPanel panel = new JPanel();
		JButton clearBtn = new SmallButton("Clear");
		panel.add(clearBtn);
		this.add(panel,BorderLayout.SOUTH);
		clearBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				traceArea.setText("");
			}
		});
	}


	@Override
	public void writeTrace(String message) {
		traceArea.append(message + "\n");
//		traceArea.update(traceArea.getGraphics());
		traceArea.setCaretPosition(traceArea.getText().length() - 1);	
	}
}
