package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;

public class ScanEvent extends EntityEvent{
	
	public ScanEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.log("entity " + myEntity.getName() + " scanning at time " + Parser.formatTime(eventTime));
		eventTime += (20.0); //TODO should use right time
		return this;
	}
}
