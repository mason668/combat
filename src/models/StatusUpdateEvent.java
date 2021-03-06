package models;

import sim.Simulation;
import utils.Logger;

public class StatusUpdateEvent extends Event {
	
	private Simulation mySimulation;

	public StatusUpdateEvent(double time, Simulation sim) {
		super(time);
		mySimulation = sim;
	}
	
	public Event doEvent(){
		Logger.say("status event " + this.eventTime);
		this.eventTime+=20.0;//mySimulation.getScenario().getParameters().getClockUpdateTime();
		return this;
	}

}
