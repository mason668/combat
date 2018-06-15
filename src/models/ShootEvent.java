package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;

public class ShootEvent extends EntityEvent{
	
	public ShootEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.log("entity " + myEntity.getName() + 
				" shooting at time " + Parser.formatTime(eventTime));
		eventTime += (5.0); //TODO should use right time
		return this;
	}
}
