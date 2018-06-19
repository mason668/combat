package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;

public class ResupplyEvent extends EntityEvent{
	
	public ResupplyEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.log("entity " + myEntity.getName() + 
				" supplying at time " + 
				Parser.formatTime(eventTime));
		eventTime += (20.0); //TODO should use right time
		return this;
	}
}
