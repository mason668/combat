package models;

import sim.Simulation;
import utils.Logger;

public class ClockUpdateEvent extends Event {
	
	private Simulation mySimulation;

	public ClockUpdateEvent(double time, Simulation sim) {
		super(time);
		mySimulation = sim;
	}
	
	public Event doEvent(){
//		Logger.say("clock event " + this.eventTime);
		mySimulation.updateClockListeners();
		this.eventTime+=mySimulation.getScenario().getParameters().getClockUpdateTime();
		return this;
	}

}
