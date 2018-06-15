package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;

public class MoveEvent extends EntityEvent{
	
	public MoveEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.log("entity " + myEntity.getName() + 
				" moving at time " + Parser.formatTime(eventTime));
		eventTime += (5.0 * Math.random()); //TODO should use right time
		return this;
	}
}
