package models;

import sim.Simulation;
import sim.entity.Entity;

public class EntityEvent extends Event{
	
	protected Entity myEntity;
	protected Simulation mySimulation;
	
	public EntityEvent (double time, Entity entity, Simulation sim){
		super(time);
		myEntity = entity;
		mySimulation = sim;
	}

}
