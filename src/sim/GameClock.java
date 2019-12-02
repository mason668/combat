package sim;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import utils.Logger;
import utils.Parser;

public class GameClock {
	
	/*
	 * Class variables
	 */

	private long thenWall = 0;
	
	/**
	 * Store the time as double seconds
	 */
	private double clock = 0.0;
	
	private boolean clockStarted = false;
	private boolean clockPaused = false;
	
	private Scenario myScenario;
	
	/*
	 * Constructors
	 */
	
	public GameClock (Scenario scenario){
		myScenario = scenario;
	}
	
	/*
	 * Accessors and mutators
	 */
	
	/**
	 * Set the clock to the specified time in seconds.
	 * @param time
	 */
	//TODO how is this different from update?
	public void setClockSecs(double time){
		clock = time;
	}
	//TODO for completeness provide mils and mins as alternatives

	/**
	 * Get the time in seconds.
	 * @return Time.
	 */
	public double getClockSecs(){return clock;}

	/**
	 * Get the time in  milliseconds.
	 * @return Time.
	 */
	public double getClockMils(){return clock*1000;}

	/**
	 * Get the time in minutes.
	 * @return Time.
	 */
	public double getClockMins(){return clock/60.0;}

	/*
	 * Methods 
	 */
	
	/**
	 * Start the game clock.
	 */
	public void startClock(){
		if (clockStarted) return;
		setSynchPoint();
		clockStarted = true;
	}
	
	/**
	 * Return the time as a formatted string.
	 */
	public String toString(){
		return Parser.formatTime(getClockSecs());
	}
	
	/**
	 * Increment the game clock by the amount (seconds) provided. This
	 * must be > 0. If the clock is paused, it will not be incremented.
	 * @param time The number of seconds to increment the clock.
	 */
	public void incrementClockSecs(double time){
		if (time <= 0) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		double newTime = clock + time;
		synchroniseClock(newTime);
		setClockSecs(newTime);
		//setSynchPoint(newTime);
	}
	
	/**
	 * Set the clock to a new time. This time must be >0 and > the current clock.
	 * If the time is > TNEVER, it will be set to TNEVER. 
	 * @param time The time set the clock (seconds).
	 */
	public void updateClockSecs(double time){
		if (time <= 0) return;
		if (time <= clock) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		if (time > Constants.NEVER) time = Constants.NEVER;
		synchroniseClock(clock);
		setClockSecs(time);
		//setSynchPoint(time);
	}
	
	/**
	 * Synchronise the game clock with real elapsed time 
	 * using the ratio specified in the game parameters.
	 * If realtime synch is off, do nothing.
	 * @param time
	 */
	private void synchroniseClock(double time){
		if (myScenario.getParameters().getRealTimeSynch()){
			long deltaWall = System.currentTimeMillis() - thenWall;
			long deltaSim = (long) ((time - thenSim )* 1000 * 
					myScenario.getParameters().getRealTimeRatio());
			if (deltaSim < 100) return;
			while (deltaSim > deltaWall){
				try {
					Thread.sleep(10);
				} catch (Exception e){}
				deltaWall = System.currentTimeMillis() - thenWall;
			}
			setSynchPoint();
		}
	}
	
	/**
	 * Manually set a synch point. This may be necessary if an external method changes the 
	 * realtime ratio.
	 */
	//TODO it might make sense to store the ratio and on/ off flag inside the clock
	private double thenSim = -Constants.NEVER;
	public void setSynchPointa(double gameTime){
		if (gameTime > thenSim+10){
			thenSim = gameTime;
			thenWall = System.currentTimeMillis();
		}
	}
	public void setSynchPoint(){
		thenSim = this.clock;
		thenWall = System.currentTimeMillis();
	}
	
	/**
	 * Provide an action listener to control the clock.
	 * <p>The action listener will respond to the following ActionCommands: 
	 * <br> "pause" will cause the clock to pause (ie stop incrementing) or resume (ie 
	 * resume incrementing).
	 * <br> "fast" will cause the clock to double in speed.
	 * <br> "slow" will cause the clock to halve in speed.
	 * <br> "1:1" will cause the clock to revert to 1:1 realtime synch.
	 * <br> "no synch" will turn off realtime synch.
	 * @return An ActionListener.
	 */
	public ActionListener getClockController(){
		return new ActionListener(){
			public void actionPerformed(ActionEvent event) {
				String s = event.getActionCommand();
				if ( s.compareToIgnoreCase("") == 0){
				}
				else if ( s.compareToIgnoreCase("pause") == 0){
					pause();
				}
				else if ( s.compareToIgnoreCase("fast") == 0){
					fast();
				}
				else if ( s.compareToIgnoreCase("slow") == 0){
					slow();
				}
				else if ( s.compareToIgnoreCase("1:1") == 0){
					setOneToOne();
				}
				else if ( s.compareToIgnoreCase("no synch") == 0){
					noSynch();
				}
			}
		};
	}

	private void pause(){
		if (clockStarted){
			clockPaused = ! clockPaused;
			setSynchPoint();
		}
	}
	
	private void setOneToOne(){
		setSynchPoint();
		myScenario.getParameters().setRealTimeSynch(true);
		myScenario.getParameters().setRealTimeRatio(1.0);
	}
	
	private void fast(){
		setSynchPoint();
		myScenario.getParameters().setRealTimeSynch(true);
		double d = myScenario.getParameters().getRealTimeRatio();
		d = d * 0.667;
		myScenario.getParameters().setRealTimeRatio(d);
	}
	
	private void slow(){
		setSynchPoint();
		myScenario.getParameters().setRealTimeSynch(true);
		double d = myScenario.getParameters().getRealTimeRatio();
		d = d * 1.5;
		myScenario.getParameters().setRealTimeRatio(d);
	}
	
	private void noSynch(){
		myScenario.getParameters().setRealTimeSynch(false);
	}

}
