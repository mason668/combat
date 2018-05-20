package models.detection;

import java.util.Vector;

import data.csd.Sensor;
import data.managers.KeyedObject;
import data.managers.SortedList;
import data.map.Coordinate;
import models.Event;
import sim.Constants;
import sim.GameClock;
import sim.Scenario;
import sim.entity.DetectedEntity;
import sim.entity.Entity;
import sim.entity.EntityDetection;
import sim.entity.ObserverEntity;
import sim.entity.TargetList;
import sim.forces.Force;
import utils.Logger;
import utils.Parser;
import utils.Tracer;

/**
 * An Event subclass that performs the Janus target gather function.
 *
 */
public class TargetGatherEvent extends Event{
	
	private Scenario myScenario;
	private ObserverEntity myEntity;
	private GameClock gameClock;

	public TargetGatherEvent(double eventTime, 
			Scenario scenario, GameClock clock, ObserverEntity entity){
		super(eventTime);
		myScenario = scenario;
		myEntity = entity;
		gameClock = clock;
	}

	/**
	 * The method to execute processing of a target gather event.
	 */
	@Override
	public Event doEvent(){
		tracing = this.myEntity.isTracing();		
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"\nprocessing target gather ");
			Tracer.write(Tracer.DETECTION,0,"for entity " + myEntity.getID());
			Tracer.write(Tracer.DETECTION,0,"event time: " + 
					Parser.formatTime(this.getTime()) + "  " + this.getTime());
			Tracer.write(Tracer.DETECTION,0,"game clock: " + 
					Parser.formatTime(gameClock.getClock()) + 
					"  " + gameClock.getClock());
		}
		if (this.myEntity.getForce() == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,0,"observer has no valid force, unable to continue "); 
			}
			Logger.err(Logger.ERROR, "entity " + this.myEntity.getID() + " has invalid force");
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"force: " + myEntity.getForce().getName());
			Tracer.write(Tracer.DETECTION,0,"target gather cycle time: " + 
					Parser.formatTime(this.myEntity.getForce().getScanTime()) + 
					"  " + this.myEntity.getForce().getScanTime()); 
		}
		targetGather();
		if (!this.myEntity.isDead()){
			this.setTime(this.getTime() + this.myEntity.getForce().getScanTime());
			return this;
		}
		return null;
	}
	
	/**
	 * Perform the target gather process.
	 */
	private void targetGather(){ //janus/target_gather
		double maxVisibility = this.myEntity.getPlatform().getMaxVisibility();
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"observer location " + myEntity.getLocation());
			Tracer.write(Tracer.DETECTION,0,"maximum visibility " + maxVisibility + "km");
		}
		maxVisibility = maxVisibility * 5.0;
		if (tracing){
			Tracer.write(Tracer.DETECTION,0,"test visibility range " + maxVisibility + "km");
		}
		Sensor sensor = this.myEntity.getCurrentSensor();
		if (tracing){
			if (sensor == null){
				Tracer.write(Tracer.DETECTION,1,"no active sensor ");
			} else {
				Tracer.write(Tracer.DETECTION,1,"using sensor " + sensor.getName());
				Tracer.write(Tracer.DETECTION,1,"sensor band " + 
						Sensor.SENSOR_BANDS[sensor.getBand()]);
			}
		}
		Vector<EntityDetection> previousEnemies = listPreviousDetections(sensor); //TODO change to a hashmap i think
		SortedList sortedList = listTargets(this.myEntity,maxVisibility, sensor);
		addTargets(sortedList, previousEnemies);
	}
	
	/**
	 * Get the list of currently detected entities to ensure they make it into the final target list.
	 * @param sensor The observer's sensor.
	 * @return A list of currently detected entities.
	 */
	private Vector<EntityDetection> listPreviousDetections(Sensor sensor){ // list_previous_detections
		Vector<EntityDetection> tempList = new Vector<EntityDetection>();
		if (sensor == null){
			return tempList;
		}
		if (sensor != null){
			if (sensor.getBand() == Sensor.BAND_MMR){
				if (tracing){
					Tracer.write(Tracer.DETECTION,0,"mmr sensors do not perform target gather");
				}
				return tempList;
			}
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"compiling list of previous detections ");
			Tracer.write(Tracer.DETECTION,2, "could detect " + this.myEntity.getDetectionList().size());
		}
		//NOTE this is looking at actual detections, not previous targets.
		//It ensures that detected entities are always in the next target gather list.
		for (EntityDetection detection: this.myEntity.getDetectionList()){
			DetectedEntity target = detection.getEntity();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"can see " + target.getName() +
						" level " + 
						Constants.OBSERVATION_LEVEL[detection.getObservationLevel()]);
			}
			if (detection.getObservationLevel()>Constants.OBSERVATION_LEVEL_UNSEEN) {
				tempList.add(detection);
			}
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"used to be able to see " +
					tempList.size() + " targets");
		}
		return tempList;
	}
	
	/**
	 * Search all targets within range and sort them by weighted priority.
	 * @param observer The observer entity.
	 * @param vismax The maximum range within which to include targets.
	 * @param sensor The observer's sensor.
	 * @return A sorted list of potential targets.
	 */
	private SortedList listTargets(ObserverEntity observer, double vismax, Sensor sensor ){
		SortedList list = new SortedList();
		double xmin = observer.getLocation().getX() - vismax;
		double xmax = observer.getLocation().getX() + vismax;
		double ymin = observer.getLocation().getY() - vismax;
		double ymax = observer.getLocation().getY() + vismax;
		
		DetectedEntity target = this.myScenario.getEntityList().findLowestX(xmin);
		if (target == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,1,"no entities to detect ");
			}
			return list; // no entities within range
		}
		int pointer = ((Entity)target).getXarrayPointer();
		/*
		 * TODO not really needed in the trace
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"found starting entity: " + target.getName());
			Tracer.write(Tracer.DETECTION,1," location " + target.getLocation().toString() +
					" pointer " + pointer);
		}
		*/
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"looking for potential targets ");
		}
		boolean inRange = true;
		while (inRange){
			double weight = testTarget(observer, xmin, xmax, ymin, ymax, target, sensor);
			if (weight>=0){
				list.add(new KeyedObject(weight,(Entity)target));
			}
			
			target = this.myScenario.getEntityList().getNext(pointer);
			if (target == null) inRange = false;
			pointer++;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,1,"found " + list.size() + " potential targets");
		}
		return list;
	}
	
	/**
	 * Get the weighted priority for this target, so it can be sorted.
	 * @param observer The observing entity.
	 * @param xmin The minimum x value within which a target can be detected.
	 * @param xmax The maximum x value within which a target can be detected.
	 * @param ymin The minimum y value within which a target can be detected.
	 * @param ymax The maximum y value within which a target can be detected.
	 * @param target The target to assess.
	 * @param sensor The sensor used by the observer.
	 * @return The weighted priority for this target or -1 if it is not to be included. 
	 */
	private double testTarget(ObserverEntity observer, 
			double xmin, double xmax, 
			double ymin, double ymax, 
			DetectedEntity target, Sensor sensor){
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"testing " +
					target.getName() + " location " + target.getLocation().toString());
			Tracer.write(Tracer.DETECTION,3,"platform type " + target.getPlatform().getName() );
		}
		if ( observer.getForce() == target.getForce()){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target and observer in the same force " );
			}
			return -1.0;
		}
		if ( target.getForce() == null){
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target has no valid force, unable to detect " );
			}
			return -1.0;
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"target force: " + target.getForce().getName());
		}
		double x = target.getLocation().getX();
		if (x < xmin) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"outside visibility box " );
			}
			return -1.0;
		}
		if (x > xmax) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"outside visibility box " );
			}
			return -1.0;
		}
		double y = target.getLocation().getY();
		if (y < ymin) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"outside visibility box " );
			}
			return -1.0;
		}
		if (y > ymax) {
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"outside visibility box " );
			}
			return -1.0;
		}
		double distance = Coordinate.distance2D(observer.getLocation(), target.getLocation());
		double range = distance;
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"range to target " + distance );
		}
		if ( target.isDead()){
			distance = distance * 5.0; // increase range for dead things to put them at the end of the list
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target is dead, increasing distance " + distance );
			}
		} else { // target is still alive
			if (observer.getSpotTarget() == target) {
				distance = 0.0;
				if (tracing){
					Tracer.write(Tracer.DETECTION,3,"the observer has been ordered to spot this target " );
				}
			}
			if (target.getPlatform().getFlyerType() != null){
				distance = distance * 0.5;
				if (tracing){
					Tracer.write(Tracer.DETECTION,3,"target is an aircraft, decreasing range " + distance );
				}
			} else { 
				// not an aircraft
				if (tracing){
					Tracer.write(Tracer.DETECTION,3,"target is not an aircraft" );
				}
				double realSize = target.getRealSize();
				int contrastClass = target.getContrastClass();
				if (tracing){
					Tracer.write(Tracer.DETECTION,3,"target actual size " + realSize );
					Tracer.write(Tracer.DETECTION,3,"target contrast class " + contrastClass );
				}
				if (contrastClass <= 0){
					Logger.err(Logger.ERROR, this.getClass().getCanonicalName() +
							": invalid contrast class: " + 
							target.getPlatform().getName() );
					return -1;
				}
				if (sensor != null){
					if (sensor.getBand() >= Sensor.BAND_OPTICAL1 && 
							sensor.getBand()<= Sensor.BAND_THERMAL2){
						double value = 
								this.myScenario.getAcquisitionMatrix().getAcquisitionThreshold(
										this.myScenario.getEntityList(),observer, target);
						double bars = this.myScenario.getAcquisitionMatrix().pairs(sensor,contrastClass, range, realSize, 0.0);
						if (tracing){
							Tracer.write(Tracer.DETECTION,3,"initial bars " + bars );
							Tracer.write(Tracer.DETECTION,3,"threshold value " + value );
						}
						if (bars < value){
							distance = distance * 2;
							if (tracing){
								Tracer.write(Tracer.DETECTION,3,"probability of detecting this target is quite low" );
								Tracer.write(Tracer.DETECTION,3,"weighted value set to " + distance );
							}
						} else { //reasonable chance to detect
							if (target.getCurrentSpeed() > 0.0){
								distance = distance * 0.5;
								if (tracing){
									Tracer.write(Tracer.DETECTION,3,"target is moving" );
									Tracer.write(Tracer.DETECTION,3,"weighted value set to " + distance );
								}
							}
						}
					}
				}
			}
		}
		int hostility = observer.getForce().getHostility(target.getForce().getName());
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"target is " + Constants.HOSTILITY_LEVEL[hostility] );
		}
		if (hostility == Constants.HOSTILITY_FRIEND){
			if (this.myScenario.getParameters().getDoSeeOwn() == Constants.DETECT_FRIENDS_ALWAYS){
				distance = -1;
			} else {
				int level = this.myEntity.getForce().getDetectionLevel(target);
				if (level >= Constants.OBSERVATION_LEVEL_IDENTIFIED){
					distance = distance * 5;
					if (tracing){
						Tracer.write(Tracer.DETECTION,3,"target is an identified friend, weighting set to " + distance );
					}
				}
			}
		}
		int priority = observer.getPlatform().getDetectionPriority(target.getPlatform());
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"detection priority " + priority );
		}
		int detectionLevel = observer.getDetectionLevel(target);
		if (detectionLevel >= Constants.OBSERVATION_LEVEL_IDENTIFIED){
			// if already identified by this force, drop its priority
			priority = Math.max(0, priority-1);
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"target has already been identified, priority " + priority );
			}
			if (priority > 0){
				distance = distance / priority;
			} else {
				distance = distance * 2.0;
			}
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,3,"weighted range = " + distance );
		}
		return distance;
	}
	
	/**
	 * Combine the list of previous targets with the list of new targets sorted by 
	 * weighted priority into one list and save that as the new entity target list.
	 * @param sortedList The list of new targets.
	 * @param previousList The list of old targets.
	 */
	private void addTargets(SortedList sortedList, Vector<EntityDetection> previousList){
		TargetList targets = new TargetList();
		
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"maximum number of targets for list " + 
					this.myEntity.getForce().getNumberOfTargets());
		}
		for (int i=0;i<sortedList.size();i++){
			KeyedObject object = sortedList.get(i);
			Object o = object.getObject();
			if (!(o instanceof DetectedEntity)) continue;
			DetectedEntity target = (DetectedEntity) o;
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"attempting to add target " + target.getName() );
			}
			for (EntityDetection detection: previousList){
				if (detection.getEntity() == target){
					if (tracing){
						Tracer.write(Tracer.DETECTION,3,"target was on previous list ");
					}
					previousList.remove(detection);
					break;
				}
			}
			if ((targets.getSize() + previousList.size())< 
						this.myEntity.getForce().getNumberOfTargets()) {
				targets.add(target);
				if (tracing){
					Tracer.write(Tracer.DETECTION,4,"added " );
				}
			} else {
				if (tracing){
					Tracer.write(Tracer.DETECTION,4,"target list full " );
				}
			}
		}
		if (tracing){
			Tracer.write(Tracer.DETECTION,2,"adding remaining previous targets ");
		}
		for (EntityDetection detection: previousList){
			DetectedEntity target = detection.getEntity();
			if (tracing){
				Tracer.write(Tracer.DETECTION,3,"adding " + target.getName() );
			}
			targets.add(target);
		}
		this.myEntity.setTargetList(targets);
	}
	
}
