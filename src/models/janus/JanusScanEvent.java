package models.janus;

import models.Event;
import models.ScanEvent;
import sim.GameClock;
import sim.Scenario;
import sim.Simulation;
import sim.entity.Entity;
import sim.entity.ObserverEntity;
import utils.Logger;
import utils.Parser;
import utils.Tracer;

public class JanusScanEvent extends ScanEvent {

	private Scenario myScenario;
	private ObserverEntity myEntity;
	private GameClock gameClock;

	public JanusScanEvent(double time, Entity entity, Simulation sim) {
		super(time, entity, sim);
		myScenario = sim.getScenario();
		myEntity = (ObserverEntity) entity;
		gameClock = sim.getGameClock();
	}

	@Override
	public Event doEvent(){
		tracing = this.myEntity.isTracing();		
		if (tracing){
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"\nprocessing target gather ");
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"for entity " + myEntity.getID());
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"event time: " + 
					Parser.formatTime(this.getTime()) + "  " + this.getTime());
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"game clock: " + 
					Parser.formatTime(gameClock.getClockSecs()) + 
					"  " + gameClock.getClockSecs());
		}
		if (this.myEntity.getForce() == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"observer has no valid force, unable to continue "); 
			}
			Logger.err(Logger.ERROR, "entity " + this.myEntity.getID() + " has invalid force");
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"force: " + myEntity.getForce().getName());
			Tracer.write(Tracer.DETECTION,Tracer.COURSEST,"target gather cycle time: " + 
					Parser.formatTime(this.myEntity.getForce().getScanTime()) + 
					"  " + this.myEntity.getForce().getScanTime()); 
		}
//		targetGather();
		if (!this.myEntity.isDead()){
			this.setTime(this.getTime() + this.myEntity.getForce().getScanTime());
			return this;
		}
		return null;
	}
}
