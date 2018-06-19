package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;

public class CasualtyEvent extends EntityEvent{
	
	public CasualtyEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.log("entity " + myEntity.getName() + 
				" casualty at time " + 
				Parser.formatTime(eventTime));
		eventTime += (20.0); //TODO should use right time
		return this;
	}
}
