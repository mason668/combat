package models;

import java.lang.reflect.Constructor;

import models.movement.MoveEventOld;
import sim.GameClock;
import sim.Scenario;
import sim.Simulation;
import sim.entity.Entity;
import sim.entity.MoverEntity;
import utils.Logger;

public class EventFactory {
	
	//*** movement model
	
	public static MoveEvent makeMoveEvent(Entity entity, Simulation sim){
		Class<?> eventClass = makeEventClass(getMoveEventName(entity, sim));
		if (eventClass == null) return null;
		MoveEvent event = null;
		double time = getTime(sim.getGameClock().getClock(),
				sim.getScenario().getParameters().getMovementCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof MoveEvent){
			event = (MoveEvent) object;
		}
		return event;
	}
	
	private static String getMoveEventName (Entity entity, Simulation sim){
		//Logger.log("a "+ entity.getMovementModel());
		if (entity.getMovementModel().compareTo("")!=0){
			return entity.getMovementModel();
		}
		/*
		if (entity.getPlatform().getMovementModel().compareTo("")!=0){
			return entity.getPlatform().getMovementModel();
		}
		*/
		return "models.MoveEvent";
	}
	
	//*** detect model
	
	public static DetectEvent makeDetectEvent(Entity entity, Simulation sim){
		Class<?> eventClass = makeEventClass(getDetectEventName(entity, sim));
		if (eventClass == null) return null;
		DetectEvent event = null;
		double time = getTime(sim.getGameClock().getClock(),
				sim.getScenario().getParameters().getMovementCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof DetectEvent){
			event = (DetectEvent) object;
		}
		return event;
	}
	
	private static String getDetectEventName (Entity entity, Simulation sim){
		//TODO
		return "models.DetectEvent";
	}
	
	//*** scan model
	
	public static ScanEvent makeScanEvent(Entity entity, Simulation sim){
		Class<?> eventClass = makeEventClass(getScanEventName(entity, sim));
		if (eventClass == null) return null;
		ScanEvent event = null;
		double time = getTime(sim.getGameClock().getClock(),
				sim.getScenario().getParameters().getMovementCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof ScanEvent){
			event = (ScanEvent) object;
		}
		return event;
	}
	
	private static String getScanEventName (Entity entity, Simulation sim){
		//TODO
		return "models.ScanEvent";
	}
	
	//*** shoot model
	
	public static ShootEvent makeShootEvent(Entity entity, Simulation sim){
		Class<?> eventClass = makeEventClass(getShootEventName(entity, sim));
		if (eventClass == null) return null;
		ShootEvent event = null;
		double time = getTime(sim.getGameClock().getClock(),
				sim.getScenario().getParameters().getMovementCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof ShootEvent){
			event = (ShootEvent) object;
		}
		return event;
	}
	
	private static String getShootEventName (Entity entity, Simulation sim){
		//TODO
		return "models.ShootEvent";
	}
	
	//*****

	private static Object getObject(
			Class<?> eventClass, 
			double time, 
			Entity entity, 
			Simulation sim){
		try{
			Constructor<?> con = eventClass.getConstructor(
				double.class, 
				Entity.class,
				Simulation.class);
			Object object = con.newInstance(time, entity, sim);
			//Logger.log("made object");
			return object;
		} catch (Exception e){
			Logger.err(Logger.ERROR, 
				"EventFactory: unable to make event for entity " + 
				entity.getName() + " " + eventClass.getName());
		}
		return null;
	}
	
	private static double getTime(double start, double epoch){
		return start + (Math.random() * epoch);
	}
	
	private static Class<?> makeEventClass(String modelName){
		if (modelName == null) return null;
		if (modelName.compareTo("")== 0) return null;
		Class<?> eventClass = null;
		try{
			eventClass = Class.forName(modelName);
		} catch (ClassNotFoundException e){
			Logger.err("EventFactory: ", Logger.ERROR, 
				"ClassNotFoundException " +
				modelName);
		}
		return eventClass;
	}
	
}
