package view.janus_menu;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class CounterMenu extends JPanel{
	private static final long serialVersionUID = 1L;
	
	public CounterMenu(){
		super();
		this.setBackground(Color.GREEN);
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		JPanel groupLine = new JPanel();
		JPanel reorgLine = new JPanel();
		JPanel counterLine = new JPanel();
		JPanel timerLine = new JPanel();
		JPanel gridLine = new JPanel();
		this.add(groupLine);
		this.add(reorgLine);
		this.add(counterLine);
		this.add(timerLine);
		this.add(gridLine);
		groupLine.add(new JLabel("Group"));
		reorgLine.add(new JLabel("Reorg"));
		counterLine.add(new JLabel("Counter"));
		timerLine.add(new JLabel("Timer"));
		gridLine.add(new JLabel("Grid"));
	}
}
