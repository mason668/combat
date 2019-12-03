package models;

import java.lang.reflect.Constructor;

import sim.Simulation;
import sim.entity.Entity;
import sim.forces.Force;
import utils.Logger;
/**
 * This is a utility class principally comprised of static methods.
 * <p>
 * The methods are used to create event objects. 
 *
 */
public class EventFactory {
	
	private static void error(Entity entity, String type, String eventName){
		Logger.err(Logger.WARNING, 
			"unable to create " + type + " event for entity " + 
			entity.getName() + " : " + eventName);
	}
	
	//*** movement model
	/**
	 * Creates a MoveEvent for the given entity.
	 * @param entity The entity to associate with the MoveEvent
	 * @param sim A pointer to the simulation.
	 * @return A MoveEvent or null if one was not created.
	 */
	public static MoveEvent makeMoveEvent(Entity entity, Simulation sim){
		String eventName = getMoveEventName(entity, sim);
		//Logger.log("move for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "movement", eventName);
			return null;
		}
		MoveEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getMovementCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof MoveEvent){
			event = (MoveEvent) object;
		} else {
			error(entity, "movement", eventName);
		}
		return event;
	}
	
	private static String getMoveEventName (Entity entity, Simulation sim){
		if (entity.getMovementModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getMovementModel().compareTo("")!=0){
			return entity.getMovementModel();
		}
		if (entity.getPlatform().getMovementModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getMovementModel().compareTo("")!=0){
			return entity.getPlatform().getMovementModel();
		}
		if (sim.getScenario().getMovementModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getMovementModel().compareTo("")!=0){
			return sim.getScenario().getMovementModel();
		}
		return "models.MoveEvent";
	}
	
	//*** detect model
	
	public static DetectEvent makeDetectEvent(Entity entity, Simulation sim){
		String eventName = getDetectEventName(entity, sim);
		//Logger.log("detect for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "detection", eventName);
			return null;
		}
		DetectEvent event = null;
		// default epoch time in case force not defined
		double time = 5.0;
		Force force = entity.getForce();
		if ( force != null){
			time = makeRandomTime(sim.getGameClock().getClockSecs(),
					force.getSearchTime());
		}
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof DetectEvent){
			event = (DetectEvent) object;
		} else {
			error(entity, "detect", eventName);
		}
		return event;
	}
	
	private static String getDetectEventName (Entity entity, Simulation sim){
		if (entity.getDetectModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getDetectModel().compareTo("")!=0){
			return entity.getDetectModel();
		}
		if (entity.getPlatform().getDetectModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getDetectModel().compareTo("")!=0){
			return entity.getPlatform().getDetectModel();
		}
		if (sim.getScenario().getDetectModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getDetectModel().compareTo("")!=0){
			return sim.getScenario().getDetectModel();
		}
		return "models.DetectEvent";
	}
	
	//*** scan model
	
	public static ScanEvent makeScanEvent(Entity entity, Simulation sim){
		String eventName = getScanEventName(entity, sim);
		//Logger.log("scan for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "scan", eventName);
			return null;
		}
		ScanEvent event = null;
		double time = 5.0; 
		Force force = entity.getForce();
		if ( force != null){
			time = makeRandomTime(sim.getGameClock().getClockSecs(),
					force.getSearchTime()); // first scan must be soon after game start
		}
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof ScanEvent){
			event = (ScanEvent) object;
		} else {
			error(entity, "scan", eventName);
		}
		return event;
	}
	
	private static String getScanEventName (Entity entity, Simulation sim){
		if (entity.getScanModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getScanModel().compareTo("")!=0){
			return entity.getScanModel();
		}
		if (entity.getPlatform().getScanModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getScanModel().compareTo("")!=0){
			return entity.getPlatform().getScanModel();
		}
		if (sim.getScenario().getScanModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getScanModel().compareTo("")!=0){
			return sim.getScenario().getScanModel();
		}
		return "models.ScanEvent";
	}
	
	//*** shoot model
	
	public static ShootEvent makeShootEvent(Entity entity, Simulation sim){
		String eventName = getShootEventName(entity, sim);
		//Logger.log("shoot for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "shoot", eventName);
			return null;
		}
		ShootEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getDirectFireCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof ShootEvent){
			event = (ShootEvent) object;
		} else {
			error(entity, "shoot", eventName);
		}
		return event;
	}
	
	private static String getShootEventName (Entity entity, Simulation sim){
		if (entity.getShootModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getShootModel().compareTo("")!=0){
			return entity.getShootModel();
		}
		if (entity.getPlatform().getShootModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getShootModel().compareTo("")!=0){
			return entity.getPlatform().getShootModel();
		}
		if (sim.getScenario().getShootModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getShootModel().compareTo("")!=0){
			return sim.getScenario().getShootModel();
		}
		return "models.ShootEvent";
	}
	
	public static ResupplyEvent makeResupplyEvent(Entity entity, Simulation sim){
		String eventName = getResupplyEventName(entity, sim);
		//Logger.log("resupply for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "resupply", eventName);
			return null;
		}
		ResupplyEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getResupplyCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof ResupplyEvent){
			event = (ResupplyEvent) object;
		} else {
			error(entity, "resupply", eventName);
		}
		return event;
	}
	
	private static String getResupplyEventName (Entity entity, Simulation sim){
		if (entity.getResupplyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getResupplyModel().compareTo("")!=0){
			return entity.getResupplyModel();
		}
		if (entity.getPlatform().getResupplyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getResupplyModel().compareTo("")!=0){
			return entity.getPlatform().getResupplyModel();
		}
		if (sim.getScenario().getResupplyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getResupplyModel().compareTo("")!=0){
			return sim.getScenario().getResupplyModel();
		}
		return "models.ResupplyEvent";
	}
	
	public static SuppressionEvent makeSuppressionEvent(Entity entity, Simulation sim){
		String eventName = getSuppressionEventName(entity, sim);
		//Logger.log("suppression for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "suppression", eventName);
			return null;
		}
		SuppressionEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getSuppressionCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof SuppressionEvent){
			event = (SuppressionEvent) object;
		} else {
			error(entity, "suppression", eventName);
		}
		return event;
	}
	
	private static String getSuppressionEventName (Entity entity, Simulation sim){
		if (entity.getSuppressionModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getSuppressionModel().compareTo("")!=0){
			return entity.getResupplyModel();
		}
		if (entity.getPlatform().getSuppressionModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getSuppressionModel().compareTo("")!=0){
			return entity.getPlatform().getSuppressionModel();
		}
		if (sim.getScenario().getSuppressionModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getSuppressionModel().compareTo("")!=0){
			return sim.getScenario().getSuppressionModel();
		}
		return "models.SuppressionEvent";
	}

	public static CasualtyEvent makeCasualtyEvent(Entity entity, Simulation sim){
		String eventName = getCasualtyEventName(entity, sim);
		//Logger.log("casualty for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "casualty", eventName);
			return null;
		}
		CasualtyEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getCasualtyCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof CasualtyEvent){
			event = (CasualtyEvent) object;
		} else {
			error(entity, "casualty", eventName);
		}
		return event;
	}
	
	private static String getCasualtyEventName (Entity entity, Simulation sim){
		if (entity.getCasualtyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getCasualtyModel().compareTo("")!=0){
			return entity.getCasualtyModel();
		}
		if (entity.getPlatform().getCasualtyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getCasualtyModel().compareTo("")!=0){
			return entity.getPlatform().getCasualtyModel();
		}
		if (sim.getScenario().getCasualtyModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getCasualtyModel().compareTo("")!=0){
			return sim.getScenario().getCasualtyModel();
		}
		return "models.CasualtyEvent";
	}
	
	public static DetectObstacleEvent makeDetectObstacleEvent(Entity entity, Simulation sim){
		String eventName = getDetectObstacleEventName(entity, sim);
		//Logger.log("obstacle for " + entity.getName() + " " + eventName);
		if (eventName == null) return null;
		Class<?> eventClass = makeEventClass(eventName);
		if (eventClass == null) {
			error(entity, "detect_obstacle", eventName);
			return null;
		}
		DetectObstacleEvent event = null;
		double time = makeRandomTime(sim.getGameClock().getClockSecs(),
				sim.getScenario().getParameters().getDetectObstacleCycleTime());
		Object object = getObject(eventClass, time, entity, sim);
		if (object instanceof DetectObstacleEvent){
			event = (DetectObstacleEvent) object;
		} else {
			error(entity, "detect obstacle", eventName);
		}
		return event;
	}
	
	private static String getDetectObstacleEventName (Entity entity, Simulation sim){
		if (entity.getDetectObstacleModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getDetectObstacleModel().compareTo("")!=0){
			return entity.getDetectObstacleModel();
		}
		if (entity.getPlatform().getDetectObstacleModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (entity.getPlatform().getDetectObstacleModel().compareTo("")!=0){
			return entity.getPlatform().getDetectObstacleModel();
		}
		if (sim.getScenario().getDetectObstacleModel().compareToIgnoreCase("null")==0){
			return null;
		}
		if (sim.getScenario().getDetectObstacleModel().compareTo("")!=0){
			return sim.getScenario().getDetectObstacleModel();
		}
		return "models.DetectObstacleEvent";
	}
	
	// supply, suppression, ,  
	// cas assess, 
	// detect  obs, 
	// firing?, impact, active sensors, doarty

	/**
	 * Create a new clock update event
	 * @param time The time the event is to run in seconds
	 * @param sim A pointer to the simulation
	 * @return A ClockUpdateEvent or null if one was not created.
	 */
	public static ClockUpdateEvent makeClockUpdateEvent(
			double time, Simulation sim){
		return new ClockUpdateEvent(time, sim);
	}

	public static CloudUpdateEvent makeCloudUpdateEvent(
			double time, Simulation sim){
		return null; //TODO make actual event
	}
	public static CloudUpdateEvent makeChemicalUpdateEvent(
			double time, Simulation sim){
		return null; //TODO make actual event
	}
	public static CloudUpdateEvent makeStatusUpdateEvent(
			double time, Simulation sim){
		return null; //TODO make actual event
	}
	public static SaveEvent makeSaveEvent(
			double time, Simulation sim){
		return null; //TODO make actual event
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
	
	private static double makeRandomTime(double start, double epoch){
		return start + (Math.random() * epoch);
	}
	
	private static Class<?> makeEventClass(String modelName){
		if (modelName == null) return null;
		if (modelName.compareTo("")== 0) return null;
		if (modelName.compareToIgnoreCase("null")== 0) return null;
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
