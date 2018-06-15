package sim;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import utils.Parser;

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
	}
	public double getClock(){return clock;}
	public void incrementClock(double time){
		if (time <= 0) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		if (myScenario.getParameters().getRealTimeSynch()){ //TODO make a function - see below
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
	
	public void updateClock(double time){
		if (time <= 0) return;
		if (time <= clock) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		if (time > Constants.NEVER) time = Constants.NEVER;
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
		setClock(time);
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
	
	public String toString(){
		return Parser.formatTime(getClock());
	}
	
}
