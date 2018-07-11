package view;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import utils.Parser;

public class ClockView extends JPanel implements ClockListener{
	//TODO the clock control buttons should be a separate component as they won't always go with the display
	private JTextField clockDisplay;
	private JButton pauseBtn;
	private JButton fastBtn;
	private JButton slowBtn;
	private JButton oneBtn;
	private JButton synchBtn;

	public ClockView(){
		this.setLayout(new BorderLayout());
		makeComponents();
		JPanel displayPanel = new JPanel();
		displayPanel.add(new JLabel("Time: "));
		displayPanel.add(clockDisplay);
		this.add(displayPanel, BorderLayout.NORTH);
		JPanel controlPanel = new JPanel();
		controlPanel.add(pauseBtn);
		controlPanel.add(fastBtn);
		controlPanel.add(slowBtn);
		controlPanel.add(oneBtn);
		controlPanel.add(synchBtn);
		this.add(controlPanel, BorderLayout.CENTER);
	}
	
	private void makeComponents(){
		clockDisplay = new JTextField("00:00:00:00");
		pauseBtn = new SmallButton("Pause");
		fastBtn = new SmallButton("Fast");
		slowBtn = new SmallButton("Slow");
		oneBtn = new SmallButton("1:1");
		synchBtn = new SmallButton("No Synch");
	}

	@Override
	public void updateClock(double clock) {
		clockDisplay.setText(Parser.formatTime(clock));
	}
	
	public void addActionListener(ActionListener listener){
		pauseBtn.addActionListener(listener);
		fastBtn.addActionListener(listener);
		slowBtn.addActionListener(listener);
		oneBtn.addActionListener(listener);
		synchBtn.addActionListener(listener);
	}

}
