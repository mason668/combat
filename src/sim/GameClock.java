package sim;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import utils.Parser;
import view.ClockListener;

public class GameClock {

	private long then = 0;
	private double clock = 0.0;
	private boolean clockStarted = false;
	private boolean clockPaused = false;
	
	private Scenario myScenario;
	
	public GameClock (Scenario scenario){
		myScenario = scenario;
	}
	
	public void setClock(double time){
		clock = time;
		this.updateClockListeners();
	}
	public double getClock(){return clock;}
	public void incrementClock(double time){
		if (time <= 0) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		if (myScenario.getParameters().getRealTimeSynch()){
			long deltaWall = System.currentTimeMillis() - then;
			long deltaSim = (long) (time * 1000 * myScenario.getParameters().getRealTimeRatio());
			while (deltaSim > deltaWall){
				try {
					Thread.sleep(10);
				} catch (Exception e){}
				deltaWall = System.currentTimeMillis() - then;
			}
		}
		double newTime = clock + time;
		setClock(newTime);
		setSynchPoint();
	}
	
	public ActionListener getClockController(){
		return new ActionListener(){
			public void actionPerformed(ActionEvent event) {
				String s = event.getActionCommand();
				if ( s.compareToIgnoreCase("") == 0){
				}
				else if ( s.compareToIgnoreCase("pause") == 0){
					if (clockStarted){
						clockPaused = ! clockPaused;
					}
				}
				else if ( s.compareToIgnoreCase("fast") == 0){
					myScenario.getParameters().setRealTimeSynch(true);
					double d = myScenario.getParameters().getRealTimeRatio();
					d = d * 0.667;
					myScenario.getParameters().setRealTimeRatio(d);
				}
				else if ( s.compareToIgnoreCase("slow") == 0){
					myScenario.getParameters().setRealTimeSynch(true);
					double d = myScenario.getParameters().getRealTimeRatio();
					d = d * 1.5;
					myScenario.getParameters().setRealTimeRatio(d);
				}
				else if ( s.compareToIgnoreCase("1:1") == 0){
					myScenario.getParameters().setRealTimeSynch(true);
					myScenario.getParameters().setRealTimeRatio(1.0);
				}
				else if ( s.compareToIgnoreCase("no synch") == 0){
					myScenario.getParameters().setRealTimeSynch(false);
				}
			}
		};
	}

	public void startClock(){
		setSynchPoint();
		clockStarted = true;
	}
	public void setSynchPoint(){
		then = System.currentTimeMillis();
	}
	
	private Vector<ClockListener> clockListeners = new Vector<ClockListener>();
	public void addClockListener(ClockListener listener){
		clockListeners.addElement(listener);
	}
	private void updateClockListeners(){
		for (ClockListener listener : clockListeners){
			listener.updateClock(clock);
		}
	}
	
	public String toString(){
		return Parser.formatTime(getClock());
	}
	
}
