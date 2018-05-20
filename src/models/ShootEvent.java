package models;

import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;

public class ShootEvent extends EntityEvent{
	
	public ShootEvent (double time, Entity entity, Simulation sim){
		super(time, entity, sim);
	}

	public Event doEvent(){
		Logger.say("entity " + myEntity.getName() + " shooting at time " + eventTime);
		eventTime += (5.0 * Math.random()); //TODO should use right time
		return this;
	}
}
