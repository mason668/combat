package sim.route;

import sim.Scenario;
import sim.entity.MoverEntity;
import utils.Tracer;

public class NodeStop extends Node{
	public void doActivity(MoverEntity entity, Scenario scenario){
		if (entity.isTracing()){
			Tracer.write(Tracer.MOVEMENT,5,"reached a stop node");
		}
		entity.stop();
	}

}
