package view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;

import sim.Simulation;
import utils.Logger;

public class StartView extends JPanel{
	
	private SmallButton startBtn;
	
	public StartView(){
		startBtn = new SmallButton("Start");
		this.add(startBtn);
	}
	
	public void setSimulation(Simulation sim){ // TODO need an interface for this
		startBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				Logger.say("hit start");
				sim.battle();
			}
		});
	}

}
