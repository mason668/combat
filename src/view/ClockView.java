package view;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import utils.Parser;

public class ClockView extends JPanel implements ClockListener{
	//TODO the clock control buttons should be a separate component as they won't always go with the display
	private static final long serialVersionUID = 1L;
	private JTextField clockDisplay = new JTextField("00:00:00:00");
	private JButton pauseBtn = new JButton("Pause");
	private JButton fastBtn = new JButton("Fast");
	private JButton slowBtn = new JButton("Slow");
	private JButton oneBtn = new JButton("1:1");

	public ClockView(){
		this.add(clockDisplay, BorderLayout.CENTER);
		JPanel controlPanel = new JPanel();
		controlPanel.add(pauseBtn);
		controlPanel.add(fastBtn);
		controlPanel.add(slowBtn);
		controlPanel.add(oneBtn);
		this.add(controlPanel, BorderLayout.EAST);
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
	}

}
