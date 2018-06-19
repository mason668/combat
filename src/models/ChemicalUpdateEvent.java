package models;

import sim.Simulation;
import utils.Logger;

public class ChemicalUpdateEvent extends Event {
	
	private Simulation mySimulation;

	public ChemicalUpdateEvent(double time, Simulation sim) {
		super(time);
		mySimulation = sim;
	}
	
	public Event doEvent(){
		Logger.say("chemical event " + this.eventTime);
//		mySimulation.updateClockListeners();
		// should do something similar for cloud listeners
		// TODO need a cloud update time
		this.eventTime+=20.0;//mySimulation.getScenario().getParameters().getClockUpdateTime();
		return this;
	}

}
