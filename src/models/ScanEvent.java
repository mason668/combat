package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;

public class ScanEvent extends EntityEvent{
	
	public ScanEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.say("entity " + myEntity.getName() + " scanning at time " + eventTime);
		eventTime += (5.0 * Math.random()); //TODO should use right time
		return this;
	}
}
