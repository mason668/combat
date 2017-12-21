package sim.route;

import sim.Scenario;
import sim.entity.Entity;
import sim.entity.MoverEntity;
import utils.Tracer;

public class NodeHold extends Node{
	private double holdTime = 0.0;
	public void doActivity(MoverEntity entity, Scenario scenario){
		if (entity.isTracing()){
			Tracer.write(Tracer.MOVEMENT,5,"reached a hold node");
		}
		((Entity)entity).setHold(holdTime);
	}
	
	public void setTime(double time){
		holdTime = time;
	}

}
