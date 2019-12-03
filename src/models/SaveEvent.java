package models;

import sim.Simulation;
import utils.Logger;

public class SaveEvent extends Event {
	
	private Simulation mySimulation;

	public SaveEvent(double time, Simulation sim) {
		super(time);
		mySimulation = sim;
	}
	
	public Event doEvent(){
		Logger.say("save event " + this.eventTime);
		this.eventTime+=20.0;//mySimulation.getScenario().getParameters().getClockUpdateTime();
		return this;
	}

}
