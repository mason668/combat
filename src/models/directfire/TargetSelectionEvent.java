package models.directfire;

import java.util.Vector;

import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.PlatformWeapon;
import data.csd.Weapon;
import data.map.Coordinate;
import models.Event;
import sim.Constants;
import sim.Parameters;
import sim.Scenario;
import sim.entity.DetectedEntity;
import sim.entity.Entity;
import sim.entity.EntityDetection;
import sim.entity.EntityWeapon;
import sim.entity.TargetEntity;
import utils.Parser;
import utils.Tracer;
import utils.Utils;

public class TargetSelectionEvent extends Event{
	
	// class variables to keep hold of the firing entity and 
	// the current simulation time
	private Entity myFirer; //TODO change to firerentity
	private double myClock;
	private Scenario myScenario;
	
	// class variables - set concurrently within findTarget
	private Entity newTarget; //TODO change to target entity
	private EntityWeapon newWeapon;

	// shorthand to determine whether to display trace messages
	private boolean tracing = false;

	/**
	 * Class constructor.
	 * @param eventTime the simulation time at which this event should occur.
	 * Should be no earlier than the current simulation time.
	 * @param clock the current simulation time.
	 * @param firer The entity doing the firing.
	 */
	public TargetSelectionEvent(double eventTime, 
			Scenario scenario, Entity firer){
		myFirer = firer;
		myScenario = scenario;
		myClock = scenario.getClock();
		super.setTime(eventTime);
	}
	
	/**
	 * Main entry point for the class.
	 * @return the time this event should be rescheduled or 
	 * the next event should be triggered.
	 */
	public Event doEvent(){
		tracing = myFirer.isTracing();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"\n\nsearching for new targets ");
			Tracer.write(Tracer.DIRECTFIRE,0,"for firer entity " + myFirer.getID());
			Tracer.write(Tracer.DIRECTFIRE,0,"event time: " + 
					Parser.formatTime(this.getTime()) + "  " + this.getTime());
			Tracer.write(Tracer.DIRECTFIRE,0,"game clock: " + 
					Parser.formatTime(myClock) + "  " + myClock);
		}

		// all the work is done here
		double time = this.getTime() + findTarget();
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,0,"next event time: " + 
					Parser.formatTime(time) + "  " + time);
			}
		this.setTime(time);
		return this;
	}

	/**
	 * High level logic for finding a target to engage.
	 * @return time the shot will occur
	 */
	public double findTarget(){ // based on nexttrgt.f
		// check if the firer is too busy to shoot for now
		double busyTime = evaluateFirer();
		if (busyTime > 0) {
			myFirer.clearTarget();
			return busyTime;
		}

		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,1,"selecting a target ");
		}
		
		// check if the firer has just completed a command fire mission
		stillComandFiring();
		
		// branch depending on whether we are still command firing or not
		// both branches must set newTarget and newWeapon
		if (myFirer.getCommandTarget() != null){
			commandFire();
		} else {
			scanTargets();
		}
		
		// if no target was found clear firer's targetting data
		// schedule this event again after one cycle time
		if ( newTarget == null | newWeapon == null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,1,"no target found");
			}
			myFirer.clearTarget();
			return myClock + this.myScenario.getParameters().getDirectFireCycleTime();
		}
		
		// a target was found
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,1,"found target " + newTarget.getID());
			Tracer.write(Tracer.DIRECTFIRE,1,"using weapon " + newWeapon.getWeapon().getName());
		}
		
		// calculate when the shot could be taken
		double fireTime = calculateFiringTime();

		// save the targetting data
		delay(fireTime);
		double time = myClock + fireTime;
		myFirer.shoot(time, newTarget, newWeapon);
		return fireTime;
	}
	
	/**
	 * Based on the target and weapon, calculate how long it will take to 
	 * complete aiming at the target ready to shoot.
	 * @return the aim time
	 */
	private double calculateFiringTime(){
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,1,"calculating firing time ");
			Tracer.write(Tracer.DIRECTFIRE,2,"firer rof is " + Constants.RATE_OF_FIRE[myFirer.getRateOfFire()]);
		}
		double fireTime = this.myScenario.getParameters().getDirectFireCycleTime();
		
		// account for weapon reloading.
		double reloadTime = 0.0;
		if ( newWeapon.getReloadTime() > myClock){
			reloadTime = newWeapon.getReloadTime() - myClock;
		}

		if ( (myFirer.getLastTarget() == newTarget)&&
				(myFirer.getLastWeapon() == newWeapon)){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,2,"this is an old target ");
			}
			fireTime = newWeapon.getWeapon().getNewTargetTime(myFirer.getRateOfFire());
		} else {
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,2,"this is a new target ");
			}
			fireTime = newWeapon.getWeapon().getPreFireTime(myFirer.getRateOfFire());
		}
		
		// adjust firing time to take account of heterogenous aggregation
		fireTime = fireTime * myFirer.getFiringTimeMutiplier(newWeapon);
		fireTime = fireTime + reloadTime;

		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,3,"refiring delay " + fireTime);
		}
		
		//if command firing, refire no faster than the df cycle time
		//kludge to stop users overriding time calculation
		if (myFirer.getCommandTarget() != null){
			fireTime = Math.max(this.myScenario.getParameters().getDirectFireCycleTime(), fireTime);
		}

		return fireTime;
	}
	
	/**
	 * Determine if the firer is actually ready to aim yet.
	 * If not, return the delay time.
	 * @return time at which firer will be ready to aim.
	 */
	private double evaluateFirer(){
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,1,"evaluating firer ");
		}
		
		// is firer still tracking a guided weapon
		double tof = myFirer.getTOF();
		if (tof > myClock){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,2,"time of flight: " + tof);
				Tracer.write(Tracer.DIRECTFIRE,2,"last shot was a guided weapon");
				Tracer.write(Tracer.DIRECTFIRE,2,"still tracking old target");
			}
			return tof;
		}
		
		// is firer "weapons down"
		if (myFirer.getROEState() == Constants.ROE_DOWN){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,2,
						"firer is weapons-down, so will not search for targets");
			}
			return myClock + this.myScenario.getParameters().getDirectFireCycleTime();
		}

		// is firer completing an ammunition resupply (upload)
		if (myFirer.getUploadTime()> myClock){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,2,"firer is busy uploading");
				Tracer.write(Tracer.DIRECTFIRE,2,"time of completion " + 
						Parser.formatTime(myFirer.getUploadTime()));
			}
			return myFirer.getUploadTime();
		}
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,2,"firer ok");
		}
		return 0;
	}
	
	// 
	/**
	 * check if the firer has completed a command fire mission and if so, clear it
	 */
	private void stillComandFiring(){
		if (myFirer.getCommandTarget() != null){
			if (myFirer.getCommandRounds()<=0){
				if (tracing){
					Tracer.write(Tracer.DIRECTFIRE,3,"currently set to command fire at " 
							+ myFirer.getCommandTarget().getID());
					Tracer.write(Tracer.DIRECTFIRE,3,"but all rounds have been expended");
				}
				myFirer.clearComandFire("command fire mission completed");
			}
		}
	}
	
	/**
	 * The fire has an active command fire mission, select target accordingly
	 */
	private void commandFire(){
		newWeapon = null;
		newTarget = null;
		
		// the only valid target is the command fire target
		Entity target = myFirer.getCommandTarget();
		int detectionLevel = myFirer.getDetectionLevel(target);
		
		// cancel mission if no longer visible
		if (detectionLevel < Constants.OBSERVATION_LEVEL_DETECTED){
			myFirer.clearComandFire("command fire target no longer visible");
			return;
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,2,"command firing at " 
					+ target.getID());
			Tracer.write(Tracer.DIRECTFIRE,4,"rounds remaining " +
					myFirer.getCommandRounds());
		}
		
		double range = Utils.range(myFirer, target);
		
		// check it is still possible to engage target
		if (evaluateTarget(target, range)) {
			newWeapon = pickWeapon(target, detectionLevel, range);
		}
				
		if ( newWeapon == null){
			myFirer.clearComandFire("No longer able to engage target");
			newTarget = null;
			newWeapon = null;
		} else {
			newTarget = target;
		}
	}
	
	/**
	 * check if this is a viable target
	 * @param target
	 * @param range
	 * @return
	 */
	private boolean evaluateTarget(TargetEntity target, double range){
		// based on picktarget.f

		// is the target already destroyed
		if ( target.getNumberOfElements()<=0){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"target is already dead "); 
			}
			return false;
		}
		
		// is the target mounted
		if (target.getCarrier() != null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"target is mounted "); 
			}
			return false;
		}
		
		// is the target within max weapon range
		if ( range > myFirer.getPlatform().getPrimaryRange()){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"target range "
						+ range 
						+ " is beyond max range " +
						myFirer.getPlatform().getPrimaryRange() ); 
			}
			return false;
		}

		// if this is a command fire mission, ignore other constraints
		if (myFirer.getCommandTarget() != null){
			return true;
		}

		// check that the user hasn't manually set some range restrictions
		if (range > myFirer.getUserMaxRange()){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"target range "
						+ range 
						+ " is beyond user set max range " +
						myFirer.getUserMaxRange() ); 
			}
			return false;
		}
		if (range < myFirer.getUserMinRange()){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"target range "
						+ range 
						+ " is within user set min range " +
						myFirer.getUserMinRange() ); 
			}
			return false;
		}
		
		return true;
	}
	
	// 
	/**
	 * based on the target selected, pick an appropriate weapon
	 * 
	 * @param target
	 * @param detectionLevel
	 * @param range
	 * @return
	 */
	private EntityWeapon pickWeapon(TargetEntity target, int detectionLevel, double range){
		// based on picktarget.f
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,4,"target ok ");
			Tracer.write(Tracer.DIRECTFIRE,4,"selecting a weapon ");
		}
		
		// get the default weapon for this target based on range
		EntityWeapon weapon = whichWeapon(target, detectionLevel, range);
		if (weapon == null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"unable to select a weapon ");
			}
			return null;
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,4,"a valid weapon has been selected " +
					weapon.getWeapon().getName());//TODO tm2
		}
		
		return weapon;
	}
	
	/**
	 * Select a weapon to fire at the specified target
	 * @param target
	 * @return
	 */
	private EntityWeapon whichWeapon(TargetEntity target, int detectionLevel, double range){
		EntityWeapon weapon = null;
		
		// if command firing use the command fire weapon
		if (myFirer.getCommandTarget() != null){
			weapon = myFirer.getCommandWeapon();
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"selected command weapon ");
			}
			
		// if user has manually selected a weapon, use it
		} else if (myFirer.getManualWeapon() != null){
			weapon = myFirer.getManualWeapon();
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,
						"user manually selected weapon " 
						+ weapon.getWeapon().getName());
			}
		// get the default weapon for this type of target 
		} else {
			weapon = autoSelect(target.getPlatform().getName(),detectionLevel, range);
		}

		// if we couldn't pick a weapon, back out
		if (weapon == null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,4,"unable to select a weapon");
			}
			return null;
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,4,"selected weapon " + weapon.getWeapon().getName());
		}
		
		testWeapon (weapon, range);
		if (!checkAmmo(weapon)){
			weapon = tryAlternateWeapon(weapon, range);
		}
		return weapon;
	}
	
	/**
	 * select the weapon assigned in the database for this target
	 * @param targetName
	 * @param detectionLevel
	 * @param range
	 * @return
	 */
	private EntityWeapon autoSelect(String targetName, int detectionLevel, double range){
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"attempting to match weapon with target ");
		}
		// if the firer is unable to recognise the type of target, then it can't select a weapon
		if ( detectionLevel < Constants.OBSERVATION_LEVEL_RECOGNISED){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"target is not recognised, unable to select a aweapon");
			}
			return null;
		}

		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"target range " + range);
		}

		// work out the default weapon for this firer type against the target at this range
		PlatformWeapon platformWeapon = 
				myFirer.getPlatform().getWeaponChoice(targetName, range);
		if (platformWeapon == null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,
						"firer does not have a weapon assigned to this target");
			}
			return null;
		} 
			if (platformWeapon.getWeapon() == null){ // this shouldn't be possible, but just in case
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer does not have a weapon assigned for this range");
			}
			return null;
		}
		
		// it looks like there is an assigned weapon
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"found weapon " + platformWeapon.getWeapon().getName());
		}
		
		// find the corresponding weapon in the list of weapons actually carried by the firer entity
		EntityWeapon entityWeapon = myFirer.getEntityWeapon(platformWeapon);
		
		// it should not be possible not to find a match, but just in case
		if (entityWeapon == null){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,
						"there seems to be a problem");
				Tracer.write(Tracer.DIRECTFIRE,5,
						"this entity does not seem to have that weapon");
			}
			return null;
		}
		return entityWeapon;
	}
	
	/**
	 * test if this weapon is able to be fired
	 * @param entityWeapon
	 * @param range
	 * @return
	 */
	private boolean testWeapon(EntityWeapon entityWeapon, double range){
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"testing wpn " + entityWeapon.getWeapon().getName());
		}

		// if the weapon really only represents an ammunition store (used by logistics model)
		// it can't be fired
		if ( entityWeapon.getWeaponSlot().getAmmoOnly()){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"weapon is ammo only ");
			}
			return false;
		}

		// see if the weapon is reloading
		if ( entityWeapon.getReloadTime() > myClock){
			if ( myFirer.getCommandTarget() != null){
			} else	if ( myFirer.getManualWeapon() != null){
			} else {
				if (tracing){
					Tracer.write(Tracer.DIRECTFIRE,5,"weapon is reloading ");
				}
				return false;
			}
		}
		
		Weapon csdWeapon = entityWeapon.getWeapon();

		// is the target beyond range for this weapon
		if ( range > csdWeapon.getMaxRange()){
			return false;
		}

		// no reason not to use this weapon
		return true;
	}
	
	/**
	 * Check if the weapon still has ammunition
	 * @param weapon
	 * @return true if ammunition remaining
	 */
	private boolean checkAmmo(EntityWeapon weapon){
		if (weapon.getCurrentAmmo()<0){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"weapon out of ammunition ");
			}
			if ( myFirer.getManualWeapon() == weapon){
				if (tracing){
					Tracer.write(Tracer.DIRECTFIRE,5,"cancel manual weapon selection, reverting to automatic ");
				}
			}
			return false;
		}
		return true;
	}
	
	/**
	 * When a primary weapon is out of ammunition, switch to an alternate one
	 * @param weapon
	 * @return alternate weapon if valid, null if not
	 */
	private EntityWeapon tryAlternateWeapon(EntityWeapon weapon, double range){
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"weapon out of ammo, trying alternate ");
		}
		weapon = weapon.getAlternateWeapon();
		if ( weapon == null) return weapon;
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"using weapon " + weapon.getWeapon().getName());
		}
		if (!checkAmmo(weapon)) {
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"alternate weapon has no ammo ");
			}
			return null;
		}
		if (testWeapon(weapon,range)){
			return weapon;
		} else {
			return null;
		}
	}
	
	/**
	 * setup a movement delay for the firer if this weapon 
	 * requires it to slow or stop to shoot
	 */
	private void delay(double fireTime){ // from nexttrgt
		int mode = newWeapon.getWeapon().getFireOnTheMove();
		if (mode == Weapon.FIRE_ON_THE_MOVE_BEFORE_IMPACT){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer must stop to shoot");
			}
			myFirer.setDelay(fireTime + this.myClock);
		}
		if (mode == Weapon.FIRE_ON_THE_MOVE_AFTER_IMPACT){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer must stop until impact");
			}
			myFirer.setDelay(fireTime + this.myClock);
		}
		if (mode == Weapon.FIRE_ON_THE_MOVE_SLOW){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer must slow to shoot");
			}
			myFirer.setSlowToShoot(true); //TODO how long does this last?
		}
	}

	/**
	 * Search through the firer's list of detections and 
	 * pick one of then to shoot at
	 * 
	 * Set the object variables newTarget and newWeapon
	 */
	private void scanTargets(){
		// needs to set newTarget and newWeapon
		newTarget = null;
		newWeapon = null;
		
		// make some local lists to remember the valid targets 
		Vector<Entity> targetList = new Vector<Entity>();
		Vector<EntityWeapon> weaponList = new Vector<EntityWeapon>();
		Vector<Double> priorityList = new Vector<Double>();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,2,"scanning for potential targets ");
		}

		// as we go, accumulate the sum of the weighted priority so we can 
		// use that to randomly select a target
		double sum = 0.0;

		// test all the potential targets
		for (EntityDetection detection : myFirer.getDetectionList()){
			DetectedEntity target = detection.getEntity();
			//TODO we do lots of casing to Entity, can we do that better?
			
			// can it be recognised to decide which weapon to use 
			int observationLevel = detection.getObservationLevel();
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,3,"testing " + target.getName() 
						+ " : " + Constants.OBSERVATION_LEVEL[
								observationLevel]);
			}
			if (observationLevel < Constants.OBSERVATION_LEVEL_RECOGNISED) continue;

			// is it within range
			double range = Coordinate.distance2D(myFirer.getLocation(), target.getLocation());
			
			if (!evaluateTarget((Entity)target, range)) continue;

			// what weapon should be used at this range
			EntityWeapon entityWeapon = pickWeapon((Entity)target, observationLevel, range);
			if (entityWeapon == null) continue;

			// get the weighted priority for this target type
			double priority = getPriority((Entity)target, observationLevel, range, entityWeapon);
			if (priority <= 0)continue;

			// at this point it looks like a potential target, add it to the list
			targetList.addElement((Entity)target);
			weaponList.addElement(entityWeapon);
			priorityList.addElement(priority);	
			sum = sum + priority;
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,3,"number of potential targets " + targetList.size());
		}

		// if there are no potential targets, exit
		if ( targetList.size() <1){
			return;
		}
			
		// if there is only one potential target, choose it
		if ( targetList.size() ==1){
			newTarget = targetList.elementAt(0); 
			newWeapon = weaponList.elementAt(0);
			return;
		}
			
		// randomly pick a target based on the relative weighted priorities
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,4,"selecting a target ");
		}
		double pointer = Math.random()* sum;
		double counter = 0;
		for (int i=0;i<targetList.size();i++){
			counter = counter + priorityList.elementAt(i);
			if (pointer<=counter){
				newTarget = targetList.elementAt(i); 
				newWeapon = weaponList.elementAt(i);
				return;
			}
		}
	}
	
	/**
	 * determine the weighted priority for this target
	 * @param target
	 * @param detectionLevel
	 * @param range
	 * @param csdWeapon
	 * @return
	 */
	private double getPriority(Entity target, int detectionLevel, double range, EntityWeapon entityWeapon){
		Weapon csdWeapon = entityWeapon.getWeapon();
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,4,"calculating weighted priority for target " + target.getPlatform().getName());
			Tracer.write(Tracer.DIRECTFIRE,5,
					"firer threhshold priority " 
					+ myFirer.getUserTargetPriority());
			Tracer.write(Tracer.DIRECTFIRE,5,
					"firer threhshold ph " 
					+ myFirer.getUserPHThreshold());
		}
		
		// get the priority for this target
		double priority = myFirer.getPlatform().getTargetPriority(target.getPlatform().getName());
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"target base priority " + priority);
		}
		if (priority < myFirer.getUserTargetPriority()){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"target priority less than threshold");
			}
			return 0.0;
		}
		if ( detectionLevel < Constants.OBSERVATION_LEVEL_IDENTIFIED){
			if (priority > 1.0) priority = priority - 1;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"observation level low, priority reduced " + priority);
			}
		} else {
			if (target.getNonFirers()>0){
				priority = priority * 0.5;
			}
			if (target.getNonMovers()>0){
				if (priority > 1.0) priority = priority - 1;
			}
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,
						"target has " + target.getNonFirers() + " firepower kills and " +
						target.getNonMovers() + " mobility kills");
				Tracer.write(Tracer.DIRECTFIRE,5,"priority adjusted " + priority);
			}
		}
		
		// check the roe against this target
		if (! myFirer.canAim(target)){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,
						"ROE won't allow the target to be aimed at");
			}
			return 0.0;
		}
		if (! myFirer.canShoot(target)){
			if (priority > 1.0) priority = 1.0;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"ROE won't allow the target to be engaged" +
						"\n\rpriority reduced " + priority +
						"\n\rnote, target may still be aimed at");
			}
		}

		int phPosture = Utils.getPHPosture(range, myFirer, target);
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"ph posture " + PHTable.getPostureName(phPosture));
		}
		double ph = csdWeapon.getPlatformPH(target.getPlatform().getName()).getProb(phPosture, range);
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"estimated ph " + ph);
		}
		if ( ph < myFirer.getUserPHThreshold()){
			return 0.0;
		}

		priority = priority * priority * ph;
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"weighted priority " + priority);
		}
		
		// check if it is possible to do further damage to the target
		if (!canDamage(target, csdWeapon, range, phPosture)){
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"weapon is unable to damage target");
			}
			return 0.0;
		}

		if ( myFirer.getLastTarget() == target){
			priority = priority * priority;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"this was the previous target, priority increased");
				Tracer.write(Tracer.DIRECTFIRE,5,"weighted priority " + priority);
			}
		}
		if (target.getLastFireTime()> myClock - 0.333){
			priority = priority * priority;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"target has recently fired, priority increased");
				Tracer.write(Tracer.DIRECTFIRE,5,"weighted priority " + priority);
			}
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"can weapon fire on the move?  "+
					Weapon.fireOnTheMoveText[csdWeapon.getFireOnTheMove()]);
		}
		if (csdWeapon.getFireOnTheMove() != Weapon.FIRE_ON_THE_MOVE_YES ){
			priority = priority * 0.5;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer would be delayed if firing on this target ");
				Tracer.write(Tracer.DIRECTFIRE,5,"priroty reduced " + priority);
			}
		}
		
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,5,"weapon setup time  "+
					entityWeapon.getWeaponSlot().getSetupTime());
		}
		if ( entityWeapon.getWeaponSlot().getSetupTime() > 0.0){
			priority = priority * 0.5;
			if (tracing){
				Tracer.write(Tracer.DIRECTFIRE,5,"firer would have to setup weapon to fire on this target ");
				Tracer.write(Tracer.DIRECTFIRE,5,"priroty reduced " + priority);
			}
		}

		return priority;
	}
		
	/**
	 * confirm that the firer is still actually able to damage the target
	 */
	private boolean canDamage(Entity target, Weapon csdWeapon, double range, int phPosture){
		//TODO this multiple kills bit could be better
		int pkPosture = PKTable.getpostureFromPh(phPosture);
		if (tracing){
			Tracer.write(Tracer.DIRECTFIRE,6,target.getName());
			Tracer.write(Tracer.DIRECTFIRE,6,target.getPlatform().getName());
			Tracer.write(Tracer.DIRECTFIRE,6,csdWeapon.getName());
			Tracer.write(Tracer.DIRECTFIRE,6,"posture " + phPosture + " : " + pkPosture);
		}
		if ( pkPosture < 0) return false; // unlikely, but -1 is a possible return value
		PKTable pkTable = csdWeapon.getPlatformPK(target.getPlatform().getName());
		if ( pkTable == null) return false; // possible that some weapons don;t have a pk table
		double pmob, pfir, pcat;
		if (!this.myScenario.getParameters().getPartialKills()){
			if (target.getMovers() <=0){
				pmob = 0.0;
			} else {
				pmob = pkTable.getProb(pkPosture+4, range);
			}
			if (target.getFirers() <=0){
				pfir = 0.0;
			} else {
				pfir = pkTable.getProb(pkPosture+8, range);
			}
		} else {
			pmob = 0.0;
			pfir = 0.0;
		}
		pcat = pkTable.getProb(pkPosture+4, range);
	
		if ((pcat+pmob+pfir>0.0)) return true;
		return false;
	}	
	
}
