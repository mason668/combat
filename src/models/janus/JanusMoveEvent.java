package models.janus;

import data.csd.BridgeLayer;
import data.csd.Platform;
import data.map.AreaType;
import data.map.Building;
import data.map.Coordinate;
import data.map.RoadType;
import models.Event;
import models.MoveEvent;
import sim.Constants;
import sim.GameClock;
import sim.obstacles.Pit;
import sim.Scenario;
import sim.Simulation;
import sim.entity.BridgeEntity;
import sim.entity.CarrierEntity;
import sim.entity.Entity;
import sim.entity.EntityWeapon;
import sim.entity.MoverEntity;
import sim.obstacles.Obstacle;
import sim.route.Node;
import sim.route.Route;
import utils.Parser;
import utils.Tracer;

/**
 * Perform a move event using the Janus(AS) algorithm.
 */
public class JanusMoveEvent extends MoveEvent {

	// class variables to keep hold of the firing entity and 
	// the current simulation time
	private Scenario myScenario;
	private MoverEntity myEntity;
	private GameClock gameClock;
	private double myClock; // the simulation clock (external)
	private double nextMoveTime;
	private double newSpeed = 0;
	private double newDirection = 0;
	private double newDistance = 0;
	private double newDelay = 0.0;
	private Coordinate newLocation;
	private boolean didMove = false;
	
	private boolean foundObstacle = false;
	private boolean foundRiver = false;
	private boolean foundMines = false;
	
	// shorthand to determine whether to display trace messages
	private boolean tracing = false;

	/**
	 * Class constructor.
	 * @param eventTime The simulation time at which this event should occur.
	 * @param scenario The scenario containing data and runtime parameters.
	 * @param entity The entity to be moved.
	 */
	public JanusMoveEvent(double eventTime, Entity entity, Simulation sim){
		super(eventTime, entity, sim);
		myScenario = sim.getScenario();
		myEntity = (MoverEntity) entity;
		gameClock = sim.getGameClock();
	}

	/**
	 * Processes the move event.
	 */
	@Override
	public Event doEvent(){
		myClock = gameClock.getClockSecs();
		tracing = myEntity.isTracing(); //TODO check move is on too
		nextMoveTime = this.getTime();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,0,"\nprocessing movement ");
			Tracer.write(Tracer.MOVEMENT,0,"for entity " + myEntity.getID());
			Tracer.write(Tracer.MOVEMENT,0,"event time: " + 
					Parser.formatTime(this.getTime(),true) + "  " + this.getTime());
			Tracer.write(Tracer.MOVEMENT,0,"game clock: " + 
					Parser.formatTime(this.myClock, true) + "  " + this.myClock);
			Tracer.write(Tracer.MOVEMENT,0,"move cycle time: " + 
					this.myScenario.getParameters().getMovementCycleTime() + " secs");
			Tracer.write(Tracer.MOVEMENT,0,"start state ");
			traceSummary();
			Tracer.write(Tracer.MOVEMENT,0,"calculating move event: ");
		}
		
		movement();
		
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,0,"completed movement calculation ");
			Tracer.write(Tracer.MOVEMENT,0,"the entity moved " + this.didMove);
			Tracer.write(Tracer.MOVEMENT,0,"will move again at time " + 
					Parser.formatTime(this.nextMoveTime,true) + 
					"  " + this.nextMoveTime);
			Tracer.write(Tracer.MOVEMENT,0,"end state ");
			traceSummary();
		}
		this.setTime(nextMoveTime);
		if (this.didMove){
			return this;
		} else {
			return null;
		}
	}
	
	/**
	 * Write some summary data to the trace file.
	 */
	private void traceSummary(){
		Tracer.write(Tracer.MOVEMENT,1,"last moved " + 
				Parser.formatTime(this.myEntity.getLastMoved(),false) + 
				"  " + this.myEntity.getLastMoved());
		Tracer.write(Tracer.MOVEMENT,1,"current location " + 
				myEntity.getLocation().toString());
		Tracer.write(Tracer.MOVEMENT,1,"speed " +
				myEntity.getCurrentSpeed() );
		Tracer.write(Tracer.MOVEMENT,1,"mover type " + 
				Constants.MOVER_TYPE[this.myEntity.getPlatform().getMoverType()]);
		Tracer.write(Tracer.MOVEMENT,1,"engine on: " + myEntity.isEngineOn());
		Tracer.write(Tracer.MOVEMENT,1,"direction of movement " +
				this.myEntity.getDirectionMove() );
		Tracer.write(Tracer.MOVEMENT,1,"direction of view     " +
				this.myEntity.getDirectionView() );
		Tracer.write(Tracer.MOVEMENT,1,"direction of facing   " +
				this.myEntity.getDirectionFace() );
		Tracer.write(Tracer.MOVEMENT,1,"defilade state " + 
				Constants.DEFILADE_STATE[myEntity.getDefilade()]);
	}
	
	/**
	 * Top level function for movement. It determines if the entity is able to 
	 * move and initiates the move.
	 */
	//TODO what happens when an entity can't move, but later can?
	private void movement(){
		// Assume the entity didn't move
		this.didMove = false;
		
		// Test if the entity can move. If not set return values and exit
		if (!canMove()){
			nextMoveTime = Constants.NEVER;
			updateLocation(
					this.myEntity.getDirectionMove(),
					new Coordinate (this.myEntity.getLocation()),
					0.0,
					this.myEntity
			);
		} else {
			while (nextMoveTime < (this.myClock + 
					this.myScenario.getParameters().getMovementCycleTime())){
				move();
			}
		}
		// recalculate weight and volume limits and update passenger locations
		this.myEntity.updateMount();
		return;
	}
	
	/**
	 * Move the entity.
	 */
	private void move(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSER,"moving at time " + 
					Parser.formatTime(this.nextMoveTime,true) + "  " + this.nextMoveTime);
		}
		double elapsedTime = this.myScenario.getParameters().getMovementCycleTime();
		// check if entity has been delayed
		double delay = this.myEntity.getDelay();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"delayed until " + 
					Parser.formatTime(delay,true) + "  " + delay);
		}
		// if delayed, update delay data
		if (delay > this.nextMoveTime){
			elapsedTime = delayed(delay);
		// if not delayed then process the movement further
		} else {
			elapsedTime = notDelayed();
		}
		// update location based on movement outcome
		updateLocation(
				newDirection,
				newLocation,
				newSpeed,
				this.myEntity
		);
		// update fuel
		polUpdate(newSpeed, elapsedTime);
		// set the new time for this event
		this.nextMoveTime += elapsedTime;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSER,"elapsed time " + 
					Parser.formatTime(elapsedTime,true) + "  " + elapsedTime);
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSER,"next time " + 
					Parser.formatTime(this.nextMoveTime,true) + 
					"  " + this.nextMoveTime);
		}
	}
	
	/**
	 * This entity is currently delayed. Perform all necessary house keeping,
	 * and calculate the time that elapsed due to the delay.
	 * @param delay The time the entity has been delayed until.
	 * @return The mount of time that will elapse until the entity is 
	 * free to move again.
	 */
	private double delayed(double delay){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"delayed" );
		}
		this.myEntity.setEngineOn();
		this.newSpeed = 0.0;
		newDirection = this.myEntity.getDirectionMove();
		newDistance = 0;
		newLocation = new Coordinate(this.myEntity.getLocation());
		if ( this.myEntity.isUploading(this.nextMoveTime)){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"uploading until "  + 
						Parser.formatTime(myEntity.getUploadTime(),false) + 
						"  " + myEntity.getUploadTime() );
			}
		}
		for (EntityWeapon w: this.myEntity.getWeaponList()){
			if ((w.getFiredLast() + w.getWeaponSlot().getPackupTime()) > this.myClock){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,2,"waiting for weapon " +
					w.getWeapon().getName() + " to packup ");
				}
			}
		}
		return delay - this.nextMoveTime;
	}
	
	/**
	 * The entity is not delayed, so perform a move step. 
	 * @return The time that elapses due to this step of movement.
	 */
	private double notDelayed(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSER,"not delayed" );
		}
		double elapsedTime = this.myScenario.getParameters().getMovementCycleTime();
		// determine where the entity is moving to
		Coordinate objective = getObjective();
		// if nowhere to move to, update data accordingly
		if ( objective == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.COURSER,"nowhere to move to" );
			}
			newSpeed = 0.0;
			newLocation = new Coordinate(this.myEntity.getLocation());
			newDirection = this.myEntity.getDirectionMove();
			noMove(elapsedTime);
			return elapsedTime;
		}
		// if the entity is moving, calculate where it moves to
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"moving to " + 
					objective.toString() );
		}
		elapsedTime = moveStep(objective);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"moved to " +
					newLocation.toString() );
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"speed " +
					newSpeed + "kph");
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"distance " +
					newDistance + "km");
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"elapsed time " +
					elapsedTime + " secs");
		}
		
		// calculate the new direction of movement
		newDirection = calculateDirection(this.myEntity.getLocation(),newLocation);

		// if the entity moved, check if it needs to exit a weapon pit and 
		// update the defilade state 
		if ( newSpeed > 0){
			myEntity.exitPit();
			this.myEntity.setDefilade(Constants.DEFILADE_EXPOSED);
		// if the entity didn't move update the defilade state
		} else {
			if (myEntity.getPlatform().getMoverType() == Constants.MOVER_FOOT){
				myEntity.setDefilade(Constants.DEFILADE_PARTIAL);
			}
		}
		// update entity state
		this.myEntity.setLastMoved(this.nextMoveTime);
		this.myEntity.setEngineOn();
		// update class variables
		this.didMove = true;
		return elapsedTime;
	}
	
	/**
	 * Get the objective location this entity is attempting to move to.
	 * @return A coordinate identifying where the entity is trying to 
	 * move, or null if the entity has no objective.
	 */
	private Coordinate getObjective(){
		if (this.myEntity.getPlatform().getCBRType() != null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"entity is a cbr ");
				Tracer.write(Tracer.MOVEMENT,3,"status " + 
						this.myEntity.getCBRStatus() + " " +
						Constants.CBR_STATUS[this.myEntity.getCBRStatus()]
				);
				Tracer.write(Tracer.MOVEMENT,3,"time " + 
						Parser.formatTime(this.myEntity.getCBRTime(),false) + 
						"  " + this.myEntity.getCBRTime());
			}
			if (this.myEntity.getCBRStatus() != Constants.CBR_PACKED){
				return null;
			}
			if (this.myEntity.getCBRTime()>this.myClock){
				return null;
			}
		}
		if ( this.myEntity.getMoveMode() == Constants.MOVE_ASSAULT){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"in assault mode " );
			}
			if (Math.random()< 0.8){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"stationary this turn " );
				}
				return null;
			}
		}
		if ( this.myEntity.getMoveMode() == Constants.MOVE_RUSH){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"in rush mode " );
			}
			if (Math.random()< 0.6){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"stationary this turn " );
				}
				return null;
			}
		}
		if ( this.myEntity.getMoveMode() == Constants.MOVE_CAUTIOUS){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"in cautious mode " );
			}
			if (Math.random()< 0.6){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"stationary this turn " );
				}
				return null;
			}
		}
		if ( this.myEntity.getMoveMode() == Constants.MOVE_ADVANCE){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"in advance mode " );
			}
			if (Math.random()< 0.6){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"stationary this turn " );
				}
				return null;
			}
		}
		if (changeFloors()) return null;
		
		return whereTo();
	}
	
	/**
	 * Determine if the entity needs to change floors before moving.
	 * @return True if the entity did change floors.
	 */
	private boolean changeFloors(){
		int gotoFloor = this.myEntity.getGotoFloor();
		if (gotoFloor<0) return false;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"ordered to change floors to " + 
					gotoFloor);
		}
		int myFloor = this.myEntity.getFloor();
		Building b = this.myEntity.getBuilding();
		int numFloors = 0;
		if (b != null){
			//TODO report which building
		} else {
			b = this.myEntity.getRoof();
			if ( b== null){
				// not in a building or on roof?
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"not in a building or on a roof");
				}
				return false;
			}
			// TODO report which building
		}
		numFloors = b.getFloors();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"this building has " + numFloors + 
					" floors");
			Tracer.write(Tracer.MOVEMENT,4,"currently on floor " + myFloor);
			Tracer.write(Tracer.MOVEMENT,4,"moving to floor " + gotoFloor);
		}
		if ( myFloor == gotoFloor){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"already on the correct floor");
			}
			this.myEntity.clearGotoFloor();
			return false;
		}
		if ( myFloor > gotoFloor){
			this.myEntity.goDown();
		} else {
			this.myEntity.goUp();
		}
		return true;
	}
	
	/**
	 * The entity is a flier, so perform the movement calculations.
	 * @param objective The objective location for this entity.
	 */
	private void moveFlyer(Coordinate objective){
		this.newSpeed = this.myEntity.getCommandSpeed();
		double altitude = this.myEntity.getAltitude();
		double distance = Coordinate.distance2D(this.myEntity.getLocation(), 
				objective);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"entity is a flier " + 
					this.myEntity.getPlatform().getFlyerType().getName());
			Tracer.write(Tracer.MOVEMENT,4,"current location " + 
					this.myEntity.getLocation()); 
			Tracer.write(Tracer.MOVEMENT,4,"flight mode " + 
					Constants.FLIGHT_MODE[this.myEntity.getFlightMode()]); 
			Tracer.write(Tracer.MOVEMENT,4,"speed " +
					this.newSpeed); 
			Tracer.write(Tracer.MOVEMENT,4,"altitude " +
					altitude*1000 + "m"); 
			Tracer.write(Tracer.MOVEMENT,4,"elevation " + 
					this.myScenario.getMap().getElevationKM(this.myEntity.getLocation())); 
			Tracer.write(Tracer.MOVEMENT,4,"objective " + objective ); 
			Tracer.write(Tracer.MOVEMENT,4,"distance to objective " + 
					distance + "km"); 
		}
		double factor = 1.0;
		double distmax = 0.1;
		double distmove = distance;
		if ( distance > 0.0){
			if ( this.newSpeed > 150.0){
				distmax = 0.2;
			}
			factor = Math.min(1.0, distmax/distance);
			distmove = factor * distance;
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"maximum distance to move " +
						distmove); 
			}
			double dx = objective.getX() - this.myEntity.getLocation().getX();
			double dy = objective.getY() - this.myEntity.getLocation().getY();
			dx = dx * factor;
			dy = dy * factor;
			this.newLocation = new Coordinate (
					this.myEntity.getLocation().getX() + dx,
					this.myEntity.getLocation().getY() + dy);
			this.newDistance = distmove;
		} else {
			this.newLocation = new Coordinate(this.myEntity.getLocation());
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"actually moving to " +
					this.newLocation); 
		}
		// Even if flier didn't move, must recalculate altitude because it could have changed altitude.
		
		double zt = this.myScenario.getMap().getElevationKM(this.newLocation);
		Building building = this.myScenario.getMap().inBuilding(this.newLocation);
		double zf = 0.0;
		if (building != null){
			this.myEntity.setRoof(building);
			zf = building.getHeight();
			//TODO report which building
		} else {
			zf = this.myScenario.getMap().getAreaHeight(this.newLocation);
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"at new location "); 
			Tracer.write(Tracer.MOVEMENT,5,"ground height  " +
					zt); 
			Tracer.write(Tracer.MOVEMENT,5,"feature height " +
					zf); 
		}
		zt = zt + zf;
		double zn = this.myEntity.getAltitude();
		if (this.myEntity.getFlightMode()==Constants.FLY_AGL){
			zn = zt + this.myEntity.getAltitude();
		} else { // flight mode amsl
			if ( zn <= zt){
				this.myEntity.alert("climbing to avoid ground");
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,5,"climbing to avoid ground "); 
				}
				zn = zt + this.myEntity.getPlatform().getFlyerType().getMinimumAltitude();
				if (zn > this.myEntity.getPlatform().getFlyerType().getMaximumAltitude()){
					if (tracing){
						Tracer.write(Tracer.MOVEMENT,5,"altitude > max " + 
								this.myEntity.getPlatform().getFlyerType().getMaximumAltitude()); 
					}
				}
			}
		}
		
		this.newLocation.setZ(zn);
		if (this.myEntity.isConicalView()){
			this.myEntity.updateConicalView();
		}
	}
	
	/**
	 * The entity is a ground mover, so perform the appropriate movement calculation.
	 * @param objective The objective location to where the entity is moving.
	 */
	private void moveGround(Coordinate objective){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"entity is a ground mover " );
		}
		if (this.myEntity.atFirePort()!=null){
			this.myEntity.clearFirePort();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"exiting a fireport " );
			}
		}
		Coordinate location2 = new Coordinate (objective);

		if (doMove(location2)){
			if (this.myEntity.isClearingMines()){
				drawLane(this.myEntity.getLocation(),newLocation);
			}
		}
	}
	
	/** 
	 * Check if the entity was on a bridge and update data accordingly.
	 */
	private void onBridge(){
		BridgeEntity bridge = this.myEntity.onBridge();
		if ( bridge == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"not on a bridge ");
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"was on bridge " + 
						bridge.getName());
			}
			bridge.removeFromBridge(this.myEntity);
		}
	}
	
	/**
	 * Perform the move calculation.
	 * @param moveObjective The location where the entity is attempting to move to.
	 * @return True if the entity moved, otherwise false.
	 */
	private boolean doMove(Coordinate moveObjective){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"calculating how far to move ");
		}
		Coordinate currentLocation = this.myEntity.getLocation();
		// if the entity is creating an obstacle, delay it for 20 secs
		if (this.myEntity.iamDigging()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"currently busy digging ");
			}
			constrobs();
			newDelay = 20.0;
			this.myEntity.setDelay(newDelay);
			return false;
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"not digging ");
			}
		}
		// work out if we have already reached our destination
		double fullDistance = Coordinate.distance2D(currentLocation, moveObjective);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"current location " + 
					currentLocation.toString());
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"moving to " + 
					moveObjective.toString());
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"distance " + 
					fullDistance + "km");
		}
		if (fullDistance <= 0.0001){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"already there ");
			}
			return false;
		}
		// if crossing a bridge, wait for the crossing time to elapse
		onBridge();
		if (waitingForMyBridge(this.myEntity.getPlatform(), currentLocation)){
			return false;
		}

		// reduce the distance it will move in this step based on terrain scale
		double spd = this.myEntity.getPlatform().getSpeedCountry();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"cross country speed " +
					spd + "kph");
		}
		spd = spd / 3600.0;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"cross country speed " +
					spd + "kps");
		}
		double distmax = this.myScenario.getMap().getMaxStepSize();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"maximum step size due to terrain is " +
					distmax + "km");
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"estimating maximum distance to move");
		}
		calculateDistance(spd, distmax, 
				this.myScenario.getParameters().getMovementCycleTime(), 
				moveObjective);

		// deal with high resolution urban terrain
		if (processUrbanTerrain()) return false;
		
		// search for any obstacles in the path
		findObstacles();

		// if we got stopped by an obstacle, cease movement
		if (newDistance < 0.0){
			return false;
		}

		// calculate speed
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"calculating speed");
		}
		newSpeed = getSpeed(moveObjective);

		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"based on actual terrain, best speed is " +
					newSpeed + "km");
		}
		
		if (this.foundObstacle) {
			newSpeed = checkCraters(newSpeed, this.myEntity.getLocation(), this.newLocation);
		}
		
		if (newSpeed< 0.01){
			newSpeed = 0.0;
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,Tracer.COURSE,"not moving ");
			}
			return false;
		}

		// based on actual speed, calculate distance and time again
		// can't move more than 20m or longer than a move cycle time
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"calculating maximum distance to move");
		}
		distmax = 0.02;
		calculateDistance(newSpeed/3600.0, distmax, 20.0, moveObjective);

		// determine where we moved to this step
		newLocation = new Coordinate(moveObjective);
		double elev = this.myScenario.getMap().getElevationKM(newLocation);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"elevation at new location " +
					elev + "km" );
			Tracer.write(Tracer.MOVEMENT,Tracer.FINE,"entity height " +
					this.myEntity.getBaseHeight()*1000 + "m" );
		}
		elev += this.myEntity.getBaseHeight();

		newLocation.setZ(elev);

		// if required conduct 
		if ( this.foundMines || this.foundObstacle || this.foundRiver){
			engr();
		}
		
		return true;
	}
	
	private void calculateDistance(double spd, double distmax, double time, Coordinate moveObjective){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"can't move further than " + distmax + "km");
			Tracer.write(Tracer.MOVEMENT,3,"or longer than " + time + " sec");
		}
		distmax = Math.min(spd*time,distmax);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"max distance = " +
					distmax + "km");
		}

		double dx = moveObjective.getX() - this.myEntity.getLocation().getX();
		double dy = moveObjective.getY() - this.myEntity.getLocation().getY();
		this.newDistance = Math.sqrt((dx*dx)+(dy*dy));
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"distance to objective " + this.newDistance);
		}
		if (this.newDistance > distmax){
			double ratio = distmax/ this.newDistance;
			dx = dx * ratio;
			dy = dy * ratio;
			this.newDistance = distmax;
			moveObjective.setX(this.myEntity.getLocation().getX()+dx);
			moveObjective.setY(this.myEntity.getLocation().getY()+dy);
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"proposed objective is too far away");
				Tracer.write(Tracer.MOVEMENT,4,"will move to "+
						moveObjective.toString());
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"proposed move is within a bound ");
				//TODO Tracer.write(Tracer.MOVEMENT,3,"***this could be the spot to reduce speed to stay in formation");
			}
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"actual distance to be moved " +
					distmax + "km");
		}
	}
	
	/**
	 * Check if the entity's movement will cross a crater and adjust speed 
	 * if necessary.
	 * @param speed The entity's current speed.
	 * @param currentLocation The entity's current location.
	 * @param objective The location the entity is trying to reach.
	 * @return The updated speed.
	 */
	private double checkCraters(double speed, Coordinate currentLocation, Coordinate objective){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,0,"checking for craters ");
		}
		if (this.myScenario.getMap().anyCraters(currentLocation, objective)){
			speed = speed * this.myEntity.getPlatform().getSpeedCraters();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,0,"speed reduced to " + speed);
			}
		}
		return speed;
	}
	
	/**
	 * Perform one step of movement.
	 * <p>
	 * Note this method affects the object variables: newSpeed, 
	 * newDirection, newDistance, newDelay, newLocation.
	 * @param objective The location the entity is trying to reach.
	 * @return The time it takes to actually move.
	 */
	private double moveStep(Coordinate objective){	
		newSpeed = 0.0;
		newDirection = 0;
		newDistance = 0;
		newDelay = 0.0;
		newLocation = new Coordinate (this.myEntity.getLocation());
		double elapsedTime = 0.0;
		if (this.myEntity.getPlatform().getFlyerType() != null){
			moveFlyer(objective);
		} else {
			moveGround(objective);
		}
		if (newSpeed > 0.001){
			elapsedTime = newDistance * 3600.0 / newSpeed; //convert from hph to kps
			if (newDelay > 0.0){
				elapsedTime += newDelay;
				newSpeed = 0;
			}
		} else {
			if (newDelay > 0.0){
				elapsedTime = newDelay;
				newSpeed = 0;
			} else {
				elapsedTime = this.myScenario.getParameters().getMovementCycleTime();
				newSpeed = 0;
			}
		}
		return elapsedTime;
	}
	
	/**
	 * Calculate the direction of movement between the entity's current location
	 * and its new one. 
	 * @param oldLocation Where the entity starts its move.
	 * @param newLocation Where the entity ends its move.
	 * @return The direction in radians.
	 */
	private double calculateDirection(Coordinate oldLocation, Coordinate newLocation){
		double direction = 0.0;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"calculating direction of movement");
		}
		MoverEntity leader = this.myEntity.getFormationLeader();
		if (leader != null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"curently following " + leader.getID());
				Tracer.write(Tracer.MOVEMENT,3,"seting direction to leader's");
			}
			direction = leader.getDirectionMove();
		} else {
			direction = Coordinate.direction(oldLocation, newLocation);
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"direction " + direction + " radians");
			Tracer.write(Tracer.MOVEMENT,3,"direction " + 
					Parser.rad2deg(direction) + " cartesian degrees");
			Tracer.write(Tracer.MOVEMENT,3,"direction " + 
					Parser.degreesFromNorth(Parser.rad2deg(direction)) + 
					" degrees from north");
		}
		return direction;
	}
	
	/**
	 * Update the entity's fuel based on elapsed time and entity speed.
	 * @param speed The speed of the entity (kph).
	 * @param elapsedTime The amount of time the entity moved.
	 */
	private void polUpdate(double speed, double elapsedTime){
		if (this.myEntity.getPlatform().getFlyerType() == null){
			if (this.myEntity.getPlatform().getMoverType() == Constants.MOVER_FOOT) return;
			if (this.myEntity.getPlatform().getMoverType() == Constants.MOVER_NONE) return;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"updating fuel ");
		}
		if (!this.myEntity.isEngineOn()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"engine is off ");
				return;
			}
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"engine is running ");
			Tracer.write(Tracer.MOVEMENT,3,"current fuel " + this.myEntity.getCurrentFuel());
		}
		double rate = 0.0;
		if (speed > 0.0){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"moving ");
			}
			rate = this.myEntity.getPlatform().getFuelConsumptionMoving();
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"stationary ");
			}
			rate = this.myEntity.getPlatform().getFuelConsumptionStationary();
		}
		double used = rate * elapsedTime;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"fuel used " + used);
		}
		this.myEntity.consumeFuel(used);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"remaining fuel " + this.myEntity.getCurrentFuel());
			Tracer.write(Tracer.MOVEMENT,3,"capapcity " + this.myEntity.getPlatform().getFuelCapapcity());
		}
	}
	
	/**
	 * Update the entity's location and other related settings.
	 */
	private void updateLocation(
			double direction, 
			Coordinate location, 
			double speed,
			MoverEntity entity
		){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"updating location " );
		}
		
		this.myEntity.setDirectionMove(direction);
		this.myEntity.updateDirectionView(); 
		this.myEntity.updateDirectionFace(); 

		this.myEntity.setLocation(location);
		this.myEntity.setCurrentSpeed(speed);
		this.myScenario.updateEntity(entity);
		this.myScenario.getPostProcessor().writeMove(entity);
	}
	
	/**
	 * The entity didn't actually move. Update related housekeeping.
	 * @param elapsedTime The amount of time that elapsed.
	 */
	private void noMove(double elapsedTime){
		double lastMoved = this.myEntity.getLastMoved();
		double howLong = this.myClock - lastMoved;
		howLong = Math.max(howLong, lastMoved);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"last moved at time " +
					Parser.formatTime(lastMoved,false) + " " + lastMoved );
			Tracer.write(Tracer.MOVEMENT,3,"or " +
					howLong + " seconds ago" );
			Tracer.write(Tracer.MOVEMENT,3,"engine stop time " +
					this.myEntity.getPlatform().getEngineStopTime() + " seconds" );
		}
		if (howLong > this.myEntity.getPlatform().getEngineStopTime()){
			this.myEntity.setEngineOff();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"engine turned off");
			}
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"mover type " + 
					Constants.MOVER_TYPE[this.myEntity.getPlatform().getMoverType()]);
		}
		if (this.myEntity.getPlatform().getMoverType() == Constants.MOVER_FOOT) {
			if (this.myEntity.getDefilade()== Constants.DEFILADE_EXPOSED)
				this.myEntity.setDefilade(Constants.DEFILADE_PARTIAL);
		} else if (this.myEntity.getPlatform().getFlyerType() == null){	
			if (this.newDelay <= 0.0) {
				if (howLong >= this.myScenario.getParameters().getDefialdeTime()){
					if (this.myEntity.getDefilade()== Constants.DEFILADE_EXPOSED)
						this.myEntity.setDefilade(Constants.DEFILADE_PARTIAL);
				}
			} else {
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"delayed entity can't go to defilade ");
				}
			}
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"defilade state " + 
					Constants.DEFILADE_STATE[myEntity.getDefilade()]);
		}
	}
	
	/**
	 * Test if the entity can move.
	 * @return True if the entity can move.
	 */
	private boolean canMove(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,1,"testing if entity can move ");
			Tracer.write(Tracer.MOVEMENT,2,"number of elements " + 
					myEntity.getNumberOfElements());
			Tracer.write(Tracer.MOVEMENT,2,"number of non-movers " + 
					myEntity.getNonMovers());
		}

		if (this.myEntity.getNumberOfElements()<1){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"all elements killed");
			}
			return false;
		}
		if ( this.myEntity.getNonMovers()>0){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"at least one element unable to move");
			}
			return false;
		}
		
		if (this.myEntity.getPlatform().getMoverType() != Constants.MOVER_FOOT){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"current fuel " + 
						this.myEntity.getCurrentFuel() + "l");
			}
			if (this.myEntity.getCurrentFuel()<=0){
				this.myEntity.resetFuelWarning();
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,2,"insufficient fuel");
				}
				return false;
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"foot movers do not use fuel");
			}
		}
		if (this.myEntity.isMounted()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"entity is mounted in "+
						this.myEntity.getCarrier().getName());
			}
			return false;
		}
		if (this.myEntity.getNBCInoperative()>0){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,2,"entity is inoperative: " +
						Constants.INOPERATIVE_REASON[this.myEntity.getNBCInoperative()] +
						" (" + this.myEntity.getNBCInoperative() + ")");
			}
			return false;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"ok to move");
		}
		return true;
	}
	
	/**
	 * Calculate the speed the entity can move.
	 * @param moveObjective The location the entity is trying to reach.
	 * @return The best speed the entity can travel at.
	 */
	private double getSpeed(Coordinate moveObjective){
		double speed = this.myEntity.getPlatform().getSpeedCountry();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"cross country speed " + speed);
		}
		if (this.myEntity.iAmRunning()){
			speed = this.myEntity.getCommandSpeed();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"running, so speed set to " + speed);
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"move mode " + 
						Constants.MOVE_MODE[this.myEntity.getMoveMode()]);
			}
		}
		
		speed = veloc(speed, moveObjective);
		if (speed <= 0.001) return speed; 
		
		if (this.myEntity.iAmRunning()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"running, so ignoring suppression");
			}
		} else {
			if (this.myEntity.isSuppressed()){
				speed = speed * this.myEntity.getPlatform().getSuppressionFactorMove();
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"suppressed, speed set to " + speed);
				}
			} else {
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,3,"not suppressed");
				}
			}
		}
		if ( this.myEntity.firedRecently(myClock)){
			speed = speed * this.myEntity.getSlowToFire();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"fired recently, speed set to " + 
						speed);
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"has not fired recently");
			}
		}
		
		if (this.myEntity.getMOPP()){
			speed = speed * this.myScenario.getParameters().getSpeedMOPP();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"in MOPP, speed reduced to " +
						speed);
			}
		}
		
		speed = checkSmoke(speed);
		
		speed = speed * this.myScenario.getParameters().getWeatherMove();
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"weather factor " +
					this.myScenario.getParameters().getWeatherMove());
			Tracer.write(Tracer.MOVEMENT,3,"speed reduced to " +
					speed);
		}
		
		if (speed > this.myEntity.getCommandSpeed()){
			speed = this.myEntity.getCommandSpeed();
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"speed greater than command speed " + 
						this.myEntity.getCommandSpeed());
				Tracer.write(Tracer.MOVEMENT,3,"speed reduced to " +
						speed);
			}
		}
		return speed;
	}
	
	/**
	 * Calculate the best speed for the entity given the vegetation it is moving through.
	 * @param speed The current set speed.
	 * @param moveObjective The location the entity is trying to reach.
	 * @return The best speed the entity can travel at.
	 */
	private double speedDue2Veg(double speed, Coordinate moveObjective){
		AreaType areaType = this.myScenario.getMap().getAreaType(moveObjective);
		if ( areaType != null){
			double reduction = areaType.getSpeedReduction(this.myEntity.getPlatform().getMoverType()); 
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"area type " + areaType.getName());
				Tracer.write(Tracer.MOVEMENT,4,"speed reduction factor = " + reduction );
			}
			if (areaType.getWater()){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"terrain is water" );
				}
				if (!this.myEntity.getPlatform().canSwim()){
					if (tracing){
						Tracer.write(Tracer.MOVEMENT,4,"platform unable to swim");
					}
					reduction = 0.0;
				}
			}
			speed = speed * reduction;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"speed due to vegetation = " + speed );
		}
		if (this.myEntity.getPlatform().getMoverType() != Constants.MOVER_FOOT){
			if ( speed < 0.1){
				this.myEntity.stop("halted due to feature");
				return 0.0;
			}
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"foot mover can always move at least 200m/hr" );
			}
			speed = Math.max(speed, 0.2);
		}
		return speed;
	}
	
	/**
	 * Calculate the speed based on terrain.
	 * @param speed The set speed for the entity.
	 * @param moveObjective The location the entity is trying to reach.
	 * @return The best speed the entity can achieve.
	 */
	private double veloc(double speed, Coordinate moveObjective){

		double slope = this.myScenario.getMap().getSlope(this.myEntity.getLocation(), moveObjective);
		RoadType roadType = this.myScenario.getMap().getRoadType(moveObjective);
		if (roadType != null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"road type " + roadType.getName() );
			}
			double reduction = roadType.getSpeedReduction(this.myEntity.getPlatform().getMoverType());
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"road speed " + 
						this.myEntity.getPlatform().getMaxVelocity() );
				Tracer.write(Tracer.MOVEMENT,4,"reduction factor " + reduction );
			}
			speed = this.myEntity.getPlatform().getMaxVelocity() * reduction;
		} else {
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"not on road " );
			}
			double srfs = speedDue2Slope(speed, slope);
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"speed due to slope = " + srfs );
			}
			if ( srfs < 0.1){
				this.myEntity.stop("halted due to steep slope");
				return 0.0;
			}
			double srfv = speedDue2Veg(speed, moveObjective);
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"speed due to features = " + srfv );
			}
			if ( srfs < 0.1){
				this.myEntity.stop("halted due to feature type");
				return 0.0;
			}
			speed = Math.min(srfs, srfv);
		}

		if (this.myEntity.isClearingMines()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"clearing mines ");
			}
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"breaching mode " + 
						Constants.MINE_CLEAR_MODE[this.myEntity.getMineClearingMode()]);
			}
			double bspeed = this.myEntity.getPlatform().getSpeedMineClear();
			if ( slope > 50.0){
				bspeed = 0.01;
			}
			speed = Math.min(speed,  bspeed);
		}

		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"speed " + speed );
		}

		return speed;
	}
	
	/**
	 * Calculate the best speed due to slope.
	 * @param speed The current set speed.
	 * @param slope The slope.
	 * @return The speed as reduced by slope.
	 */
	private double speedDue2Slope(double speed, double slope){
		final double minSlope = 0.15;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,5,"actual slope " + slope );
			Tracer.write(Tracer.MOVEMENT,5,"minimum slope " + minSlope );
			Tracer.write(Tracer.MOVEMENT,5,"maximum slope " + 
					this.myEntity.getPlatform().getMaxSlope() );
		}
		if (slope < minSlope){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,5,"slope is < minimum " + minSlope );
				Tracer.write(Tracer.MOVEMENT,5,"ignoring slope" );
			}
			return speed;
		}
		if (this.myEntity.getPlatform().getMoverType()!= Constants.MOVER_FOOT){
			if (slope > this.myEntity.getPlatform().getMaxSlope()){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,5,"slope is > maximum " + 
							this.myEntity.getPlatform().getMaxSlope() );
				}
				return 0.0;
			}
		}
		double b1 = this.myEntity.getPlatform().getSlopeFactor();
		double srfs = speed *(1.0 + (b1*slope*100));
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,5,"platform slope factor " + b1 );
			Tracer.write(Tracer.MOVEMENT,5,"speed " + srfs + "kph");
		}
		if (this.myEntity.getPlatform().getMoverType()== Constants.MOVER_FOOT){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,5,"foot mover, min speed " + 0.2 + "kph");
			}
			srfs = Math.max(srfs, 0.2); // foot movers can always move a bit
		}
		return srfs;
	}
	
	/**
	 * Determine where the entity should move to.
	 * @return The location the entity is trying to reach or null 
	 * if it has no objective.
	 */
	private Coordinate whereToMove(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking move order");
		}
		Coordinate objective = this.myEntity.getMoveTo();
		if (objective == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no move to order");
			}
			return null;
		}

			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"move to " + objective.toString());
			}
			if (Coordinate.distance2D(objective, this.myEntity.getLocation()) 
					>0.0001){
				return this.myEntity.getMoveTo();
			}
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"reached objective, looking for other orders");
			}
			this.myEntity.setMoveTo(null);
			return objective;
	}
	
	/**
	 * Tests if the entity is following a circle order.
	 * @return Returns the objective location if the entity is circling, otherwise null.
	 */
	private Coordinate whereToCircle(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking circle order");
		}
		Coordinate objective = this.myEntity.getCircle();
		if (objective == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no circle order");
			}
			return null; 
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"circle order " + 
					objective.toString());
			Tracer.write(Tracer.MOVEMENT,4,"radius "+
					this.myEntity.getCircleRadius());
			Tracer.write(Tracer.MOVEMENT,4,"circle time " + 
					Parser.formatTime(this.myEntity.getCircleTime(),false) + 
					"  " + this.myEntity.getCircleTime());
		}
		if ((this.myEntity.getCircleTime() <= this.myClock) && 
				(this.myEntity.getCircleTime() > 0) ){ 
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"time has elapsed");
			}
			this.myEntity.clearCircle();
		} else {
			double distance = Coordinate.distance2D(
					objective, this.myEntity.getLocation());
			if ( distance > (this.myEntity.getCircleRadius()*1.1)){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"distance from centre " +
							distance);
					Tracer.write(Tracer.MOVEMENT,4,"moving toward centre ");
				}
				return objective;
			}
			// distance <= radius
			double angle = Coordinate.direction(this.myEntity.getLocation(), objective);
			if (this.myEntity.getCircleClockwise()){
				angle += 0.1; // make slightly more clockwise
			} else {
				angle -= 0.1; // make angle slightly more anti-clockwise
			}
			boolean onMap = false;
			double radius = this.myEntity.getCircleRadius();
			while (!onMap && (radius >0.0)){
				double xobj = objective.getX() + this.myEntity.getCircleRadius()*Math.cos(angle);
				double yobj = objective.getY() + this.myEntity.getCircleRadius()*Math.cos(angle);
				Coordinate result = new Coordinate(xobj,yobj);
				onMap = this.myScenario.getMap().onMap(result);
				if (onMap) {
					if (tracing){
						Tracer.write(Tracer.MOVEMENT,4,"moving around circumference " +
								result.toString());
					}
					return result;
				}
				radius = radius - 0.1;
			}
		}
		return null;
	}
	
	/**
	 * Tests if the entity has a mount order.
	 * @return The location of the carrier the entity has been ordered to mount, 
	 * or null if the entity does not have a mount order. 
	 */
	private Coordinate whereToMount(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking mount order");
		}
		CarrierEntity carrier = this.myEntity.getCommandMount();
		if (carrier==null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no mount order");
			}
			return null;
		}
		if (carrier.isDead()){
			this.myEntity.setCommandMount(null);
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"carrier is dead");
			}
			return null;
		}
		Coordinate objective = new Coordinate(carrier.getLocation());
		double delay = this.myScenario.getParameters().getMovementCycleTime();
		delay = delay + this.myClock + 5.0; // add 5 secs
		carrier.setDelay(delay);
		double distance = Coordinate.distance2D(objective, this.myEntity.getLocation());
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"ordered to mount " + carrier.getName());
			Tracer.write(Tracer.MOVEMENT,4,"location " + objective.toString());
			Tracer.write(Tracer.MOVEMENT,4,"distance " + distance);
		}
		if (distance <= this.myScenario.getParameters().getRangeMountInner()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"within inner range " + this.myScenario.getParameters().getRangeMountInner());
				Tracer.write(Tracer.MOVEMENT,4,"will mount carrier ");
			}
			this.myEntity.mount(carrier);
			return null;
		}
		return objective;
	}
	
	/**
	 * Tests if the entity has a pit order.
	 * @return The location of the pit the entity has been ordered to mount, 
	 * or null if the entity does not have a pit order. 
	 */
	private Coordinate whereToPit(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking pit order");
		}
		Pit pit = this.myEntity.getCommandPit();
		if (pit==null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no pit order");
			}
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"ordered to mount pit " + pit.getLocation());
		}
		Coordinate objective = pit.getLocation();
		double distance = Coordinate.distance2D(this.myEntity.getLocation(), objective);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"distance to pit " + distance);
			Tracer.write(Tracer.MOVEMENT,4,"mounting distance " + 
					this.myScenario.getParameters().getRangePitInner());
		}
		if (distance <= this.myScenario.getParameters().getRangePitInner()){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"mounted pit");
			}
			this.myEntity.occupyPit(pit);
			return null;
		}
		return objective;
	}
	
	/**
	 * Test if the entity has a valid movement route to follow.
	 * @return The location of the first node in the route if 
	 * it has one or null if it does not.
	 */
	private Coordinate whereToRoute(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking route");
		}
		Route route = this.myEntity.getRoute();
		if (route==null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no route");
			}
			return null;
		}
		if (this.myEntity.getConvoyLeader() != null){
			if (convoyStopped()){
				return null;
			}
		}
		Node node = locateNode(route);
		if (node == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no nodes");
			}
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"moving to node " + 
					node.getLocation().toString());
		}
		return new Coordinate(node.getLocation());
	}
	
	/**
	 * Test to see if the entity is part of a formation.
	 * @return The location the entity should occupy relative to the formation leader
	 * or null if it is not part of a formation.
	 */
	private Coordinate whereToFormation(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"checking formation order");
		}
		MoverEntity leader = this.myEntity.getFormationLeader();
		if (leader==null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"no formation");
			}
			return null;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"part of a formation");
			Tracer.write(Tracer.MOVEMENT,4,"following " + leader.getID());
		}
		Coordinate objective = calculateFormation();
		return objective;
	}

	/**
	 * Determine where the entity is trying to move to.
	 * This first considers a move order, then whether the entity has been 
	 * ordered to mount, then whether it has been ordered to occupy a pit,
	 * then if it has a movement route and finally if it is part of a formation.
	 * This order determines the priority of which objective will
	 * be selected.
	 * @return A location to move to if one exists, otherwise null.
	 */
	private Coordinate whereTo(){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,2,"determining where to move");
		}
		Coordinate objective = whereToMove();
		if (objective != null) return objective;
		objective = whereToCircle();
		if (objective != null) return objective;
		
		objective = whereToMount();
		if (objective != null) return objective;
		if (this.myEntity.isMounted()) {return null;}
		
		objective = whereToPit();
		if (objective != null) return objective;
		if (this.myEntity.getPit() != null){return null;}
		
		if (this.myEntity.isStopped()){// can't move if stopped
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"entity is stopped");
			}
			return null;
		}
		checkHold();
		if (this.myEntity.isHolding()){// can't move if stopped
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,3,"entity is holding until " +
						Parser.formatTime(this.myEntity.getHoldTime()));
			}
			return null;
		}
		
		if (this.myEntity.getRoute() == null){ // no route, so try a formation
			objective = whereToFormation();
			if (objective != null) return objective;
		}

		// at this point must have a route
		objective = whereToRoute();
		if (objective != null) return objective;
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"no valid move orders");
		}
		return null;
	}
	
	
	/**
	 * Check if the hold time for this entity has elapsed.
	 */
	private void checkHold(){
		if (this.myEntity.isHolding()) {
			if (this.myEntity.getHoldTime() > 0){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"holding until " +
							this.myEntity.getHoldTime());
				}
				if (this.myEntity.getHoldTime() <= this.myClock){
					if (tracing){
						Tracer.write(Tracer.MOVEMENT,4,"hold time has elapsed " );
					}
					this.myEntity.go();
				}
			}
		}
	}

	/**
	 * Calculate where the entity should be relative to the formation leader.
	 * <p>TODO formations not yet implemented.
	 * @return A location or null if the entity is not part of a formation.
	 */
	private Coordinate calculateFormation(){
		return null;
		/*
	  IF (IAMFOLLOWING(IUNIT,ISIDE) .GT. 0 ) THEN ! part of a formation
	    IF( KFLAGNODE(1,IUNIT,ISIDE) .GT. 0 )  THEN	 ! not at stop node

D	      IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        WRITE (KLINE$,"(8x,'Icon is part of a formation.')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      CALL CALCULATE_FORM ( IUNIT, ISIDE, XOBJ, YOBJ, ZOBJ, IOK )
C	      CALL LOCATE_FORM ( IUNIT, ISIDE, XOBJ, YOBJ, ZOBJ, IOK )

	      IF ( IOK .GT. 0 ) THEN
	        CALL CHECK_MOVE_TO (IUNIT, ISIDE, X1, Y1, XOBJ, YOBJ, IOK )

	        IF ( IOK .GT. 0 ) THEN
	          PRINT *
	          PRINT *,'Hmmm, it looks like the formation settings are stuffed'
	          PRINT *,IAMFOLLOWING(IUNIT,ISIDE)
	          PRINT *,'I will fix it for this icon'
	          PRINT *
c	        CALL PRESSRET

	          write (IFILELOG,"(' ')")
	          write (IFILELOG,"('Hmmm, it looks like the formation settings are stuffed')")
	          write (IFILELOG,"('for icon ',I6,XX,I2)") IUNIT,ISIDE
	          write (IFILELOG,"('leader',I6,XX,I2)") IAMFOLLOWING(IUNIT,ISIDE), ISIDE
	          write (IFILELOG,"(' ')")

	          IAMFOLLOWING(IUNIT,ISIDE) = 0
	        ENDIF
	      ENDIF

		 */
	}
	
	/*

	SUBROUTINE LOCATE_CONVOY ( IUNIT, ISIDE, IFLYTYP, NODE,
     *				X1, Y1, XOBJ, YOBJ, ZOBJ, IPREPO )

	include 	'global.f'
	include 	'globunits.f'
	include 	'globnode.f'
	include 	'globmove.f'
	include		'globdir.f'
	include		'glbnames.f'
	include		'globrpt.f'

	IPREPO = 0
	ISUNIT = IAMCONVOYING(IUNIT,ISIDE)

D	IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$, "(8X,'Apparently my leader is icon ',I6)") ISUNIT
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF (NSCORE(ISUNIT,ISIDE) .GT. 0 ) THEN

	  XS = XUNIT(ISUNIT,ISIDE)
	  YS = YUNIT(ISUNIT,ISIDE)

D	  IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$, "(8X,'His loc is ',F12.3,XX,F12.3)") XS, YS
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  DX = X1 - XS
	  DY = Y1 - YS
	  TDIST = SQRT( DX*DX + DY*DY )

D	  IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$, "(8X,'Which is ',F12.3,'km away')") TDIST
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$, "(8X,'Convoy distance is ',F12.3,'km away')") MYCONVDIST(IUNIT,ISIDE)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL LOCATE_NODE ( IUNIT, ISIDE, IFLYTYP, NODE, IVIEW,
     *				X1, Y1, XOBJ, YOBJ, ZOBJ, IPREPO )

	  IF ( TDIST .LT. MYCONVDIST(IUNIT,ISIDE) ) THEN
				! The icon is closer to the leader than the
				! convoy distance would allow. Hold for this
				! move cycle.
	    XOBJ = X1
	    YOBJ = Y1

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$, "(8X,'Too close, staying at ',F12.3,XX,F12.3)") XOBJ, YOBJ
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ENDIF
	ELSE
	  IAMCONVOYING(IUNIT,ISIDE) = 0
	  ITYPE  =  KSYSTYP(IUNIT,ISIDE)
	  ISTYPE  =  KSYSTYP(ISUNIT,ISIDE)
	  IVIEW = 0

D	  IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$, "(8X,'Lost contact with leader')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

c	  CALL HIUNIT ( IUNIT, ISIDE, IVIEW, L )
c	  WRITE (L, 1001 ) SYSTNAME$(ISTYPE,ISIDE),
c     *		UNITID$(ISUNIT,ISIDE)
1001	  FORMAT ('lost contact with ',A16,X,A6 )
	ENDIF

900	CONTINUE
	RETURN
	END

	SUBROUTINE AM_I_FOLLOWING ( IUNIT, ISIDE, IRESULT)

	IMPLICIT NONE

	include 	'global.f'
	include 	'globunits.f'
	include 	'globnode.f'
	include 	'globmove.f'
	include		'globdir.f'

	INTEGER*4	IUNIT, ISIDE, IRESULT

	IRESULT = 0

	IF ( IOBJPTR(IUNIT,ISIDE) .GT. 0 ) THEN
	  IRESULT = 0
	  RETURN
	ENDIF

	IF ( MOVE_TO_X(IUNIT,ISIDE) .GT. 0 .AND. MOVE_TO_Y(IUNIT,ISIDE) .GT. 0 ) THEN
	  IRESULT = 0
	  RETURN
	ENDIF

	IF ( IAM_MOUNTING(IUNIT, ISIDE) .GT. 0 ) THEN
	  IRESULT = 0
	  RETURN
	ENDIF

	IF ( IAMFOLLOWING(IUNIT,ISIDE) .GT. 0 ) THEN
	  IRESULT = IAMFOLLOWING(IUNIT,ISIDE)
	ENDIF

	RETURN
	END


	SUBROUTINE CHECK_MOVE_TO (IUNIT, ISIDE, X1, Y1, XOBJ, YOBJ, IOK )

	include 	'global.f'
	include 	'globunits.f'
	include 	'globnode.f'
	include 	'globmove.f'
	include		'globdir.f'
	include		'glbnames.f'
	include		'globrpt.f'
	include		'globpi.f'

	CALL TRRNCHARS (XLL, YLL, XW, YT, IX, IY, XS, YS )

	IOK = 0

	IF ( XOBJ .LT. XLL ) THEN
	  PRINT *
	  PRINT *,'icon ',IUNIT,ISIDE,' has an invalid move_to_x ',XOBJ
	  PRINT *
	  XOBJ = X1
	  IOK = 1
	ENDIF
	IF ( XOBJ .GT. XLL+XW ) THEN
	  PRINT *
	  PRINT *,'icon ',IUNIT,ISIDE,' has an invalid move_to_x ',XOBJ
	  PRINT *
	  XOBJ = X1
	  IOK = 1
	ENDIF
	IF ( YOBJ .LT. YLL ) THEN
	  PRINT *
	  PRINT *,'icon ',IUNIT,ISIDE,' has an invalid move_to_y ',YOBJ
	  PRINT *
	  YOBJ = Y1
	  IOK = 1
	ENDIF
	IF ( YOBJ .GT. YLL+YT ) THEN
	  PRINT *
	  PRINT *,'icon ',IUNIT,ISIDE,' has an invalid move_to_y ',YOBJ
	  PRINT *
	  YOBJ = Y1
	  IOK = 1
	ENDIF


	RETURN
	END
		 */
	

	/**
	 * Check if the convoy has stopped and therefore whether this entity should stop.
	 * <p> TODO convoys not yet implemented.
	 * @return True if the entity should stop.
	 */
	private boolean convoyStopped(){
		// checkConvoy()
		/*
	  IF (IAMCONVOYING(IUNIT,ISIDE) .GT. 0 ) THEN ! part of a convoy
	    IF( KFLAGNODE(1,IUNIT,ISIDE) .GT. 0 .AND.
     *		KFLAGNODE(1,IUNIT,ISIDE) .NE. INODETRIGGER)  THEN	 ! not at stop node

D	      IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	        WRITE (KLINE$,"(8x,'Icon is part of a convoy.')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      CALL LOCATE_CONVOY ( IUNIT, ISIDE, IFLYTYP, NODE,
     *				X1, Y1, XOBJ, YOBJ, ZOBJ, IPREPO )
	      CALL CHECK_MOVE_TO (IUNIT, ISIDE, X1, Y1, XOBJ, YOBJ, IOK )

	      GOTO 900
	    ENDIF
	  ENDIF
		 * 
		 */
		return false; 
	}
	
	/**
	 * Get the location of the next node in the route.
	 * <p> TODO routes not yet implemented.
	 * @param route The route being followed.
	 * @return A location if it is valid otherwise null.
	 */
	private Node locateNode(Route route){
		/*
	IF( KFLAGNODE(1,IUNIT,ISIDE) .EQ. INODEONTRIGGER )  THEN
	  IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	    WRITE (KLINE$,"('Icon is waiting at a trigger node.')")
	    CALL TRACEOUT (0,0,0)
	  ENDIF

	  XOBJ  =  X1
	  YOBJ  =  Y1
	  GOTO 900

	ENDIF

	IF( KFLAGNODE(1,IUNIT,ISIDE) .GE. 0 )  THEN

C---------- Unit's node is a "Go " node, fetch objective node's location

	    XOBJ  =  XNODE(NODE,IUNIT,ISIDE)
	    YOBJ  =  YNODE(NODE,IUNIT,ISIDE)

	    IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	      WRITE (KLINE$, "('Node ',I4,x,f12.3,x,f12.3)" ) NODE, XOBJ, YOBJ
	      CALL TRACEOUT (0,0,0)
	    ENDIF

C---------- See if the node has been cleared, or is currently being altered.
C---------- If so, don't move.

	    IF( XOBJ .LT. 0.0 )  THEN		! Node has been cleared
		XOBJ  =  X1
		YOBJ  =  Y1
		GOTO 900
	    ENDIF
	    IF( IVIEW .GT. 0 )  THEN
		IF( NODEPTR(IVIEW) .EQ. NODE )  THEN	! Node is being altered
	          IF ( DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
	            WRITE (KLINE$, "('But this node is being altered')" )
	            CALL TRACEOUT (0,0,0)
	          ENDIF
		  XOBJ  =  X1
		  YOBJ  =  Y1
		  GOTO 900
		ENDIF
	    ENDIF
		 * 
		 */
		Node node = route.getNextNode();
		if ( node == null) return null;
		// at this point we have a valid node
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,5,"next node at " + node.getLocation().toString());
		}
		double distance = Coordinate.distance2D(
				this.myEntity.getLocation(), node.getLocation()
			);
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,5,"distance " + distance);
		}
		if (distance <= Constants.VERY_CLOSE){ 
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,5,"already at node");
			}
			node.doActivity(this.myEntity, this.myScenario);
			route.clearNode();
			if (this.myEntity.isStopped()) {
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,5,"currently stopped");
				}
				return null;
			}
			if (this.myEntity.isHolding()) {
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,5,"holding until " + 
							Parser.formatTime(this.myEntity.getHoldTime(),false));
				}
			return null;
			}
			return locateNode(route);
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,5,"haven't reached node yet");
		}
		if (node.waiting(this.myEntity, this.myScenario)){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,5,"waiting before moving on");
			}
			return null;
		}
		return node;
	}
	
	/**
	 * Process engineering functions such as mine clearing.
	 * <p> TODO engineering not yet implemented.
	 */
	private void engr(){
		 /* TODO engineering
C-----------------------------------------------------------------------C
C									C
C------ Check for obstacles in path, if present try to remove them.	C
C									C
C	IANS > 0 indicates that unit stops for some type of obstacle:	C
C			1  =  Engineer obstacle				C
C			2  =  River					C
C			3  =  Minefield					C
C			4  =  Fire					C
C		X2,Y2  is the stopping location				C
C									C
C-----------------------------------------------------------------------C

	IF( (IRIV.OR.IOBS.OR.MINES) .NE. 0 )  THEN

D	  IF ((DEBUGMOVE .GT. 0 .OR. DEBUGENGR .GT. 0 ) .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    CALL ICON_ID(IUNIT, ISIDE, KLINE$)
D	    CALL TRACEOUT (0,2,0)
D	    WRITE (KLINE$, 3005)
D	    CALL TRACEOUT (0,0,0)
3005	    FORMAT (12X,'Attempting to negotiate an obstacle.')
D	    WRITE (KLINE$,"('Travelling at speed ',F12.3,'kph')") SPEED
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CALL ENGR ( IUNIT, ISIDE, X1,Y1, X2,Y2,
    *			  TRAFFIC, IOBS, IRIV, MINES, IANS, DELAY )

C---------- Check if unit should be delayed by obstacles...

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$, "('Returned to movement processing.',I4)") IANS
D	      CALL TRACEOUT (0,2,0)
D	    ENDIF

	    IF( IANS .GT. 0 )  	THEN
		IF(  X2.EQ.X1  .AND.  Y2.EQ.Y1  )  THEN
		    SPEED    =  0.0
		    DISTMOVD  =  0.0
		    GOTO 999
		ELSE IF( IANS .EQ. 3 )  THEN	! Minefield
		    DX  =  X2 - X1
		    DY  =  Y2 - Y1
		    DISTMOVD  =  SQRT( DX*DX + DY*DY )
		ENDIF
	    ENDIF
	ENDIF
		  * 
		  */
	}
	/**
	 * Perform some work constructing an obstacle.
	 * <p> TODO engineering not yet implemented.
	 */
	private void constrobs(){
		if (this.myEntity.getConstructingObstacle() == null){
			Obstacle obstacle = new Obstacle(); // what type of obstacle?
			this.myEntity.setConstructingObstacle(obstacle);
		}
		/*
        SUBROUTINE  CONSTROBS ( IUNIT, ISIDE )


        include 'global.f'
        include 'globunits.f'
	include	'globmove.f'
        include 'globeng.f'         ! KOBTYPE, KOBSEEN,XOBST,YOBST
        include 'glbedlay.f'        ! EDELAY
	include 'globdir.f'
	include 'globpi.f'
	include 'globdis.f'

	IF ( MY_OBSTACLE(IUNIT, ISIDE) .GT. 0 ) THEN ! currently making something
	  IOBS = MY_OBSTACLE(IUNIT, ISIDE)
	  GOTO 800
	ENDIF

	! otherwise must be trying to create a new obstacle

C ----  Search for empty slot.

        DO  IOBS = 1, NUMOBST
            IF( KOBTYPE(IOBS) .LE. 0 )  GOTO  100
        ENDDO

        GOTO  999

C ----  An empty slot has been found, add the new obstacle.

 100    IOBTYPE  =  IAMDIGGING (IUNIT, ISIDE)
	MY_OBSTACLE(IUNIT, ISIDE) = IOBS

        KOBTYPE(IOBS)  =  IOBTYPE
        KOBSIDE(IOBS)  =  ISIDE
        XCTR           =  XUNIT(IUNIT,ISIDE)
        YCTR           =  YUNIT(IUNIT,ISIDE)

        KOBSEEN(IOBS,ISIDE)  =  .TRUE.

C        IF( IOBTYPE .EQ. 1 )  THEN
            KOBSTAT(IOBS)    =  1
C        ELSE
C            KOBSTAT(IOBS)    =  -2
C        ENDIF

C ----  Update maximum number of obstacles if necessary.

        IF( IOBS .GT. KMAXOBST )  KMAXOBST  =  IOBS

C ----  Rotate the obstacle to its desired orientation.

        ROT  =  DVIEW(IUNIT,ISIDE) + HALFPI
        HYPOT  =  XDIST(IOBTYPE) * 0.5

        XOBST(1,IOBS)  =  XCTR - HYPOT*COS(ROT)
        XOBST(2,IOBS)  =  XCTR + HYPOT*COS(ROT)
        YOBST(1,IOBS)  =  YCTR - HYPOT*SIN(ROT)
        YOBST(2,IOBS)  =  YCTR + HYPOT*SIN(ROT)

C ----  Set obstacle bit in affected grid cells

        X1  =  XOBST(1,IOBS)
        Y1  =  YOBST(1,IOBS)
        X2  =  XOBST(2,IOBS)
        Y2  =  YOBST(2,IOBS)

        CALL OBSBIT(X1,Y1,X2,Y2)

C ----  Set Clearing Time for the obstacle

        TOBST(IOBS)  =  0

C ----  Displace the unit

c        ROT  =  DVIEW(IUNIT,ISIDE) + HALFPI

C	print *, ROT, halfpi,pi

C        XD = HYPOT * 0.5 *COS(ROT)
C        YD = HYPOT * 0.5 *SIN(ROT)

C        PRINT *,'DISPLACED BY ',XD, YD

	X2 = XCTR !  + XD
	Y2 = YCTR !  + YD
	Z2 = 0.0
	SPEED = 0.0
	DIRECTION = DVIEW(IUNIT, ISIDE)

	CALL UPDATE_LOCATION (IUNIT, ISIDE, X2, Y2, Z2, SPEED, DIRECTION)
	IVIEW = NTFDEV(IUNIT,ISIDE)
	IREDRAW(IUNIT,IVIEW,ISIDE) = 1

C ----  Draw the obstacle

	call obsvis (iobs, 0, 1)

c	 IVIEW = NTFDEV(IUNIT,ISIDE)
C        CALL KMONITOR( KDSPPHY(IVIEW) )
C        CALL MAKEOB(IOBS)
C        CALL WTOBST(IOBS)

800	CONTINUE

	! increase obstacle capability
	TOBST(IOBS) = TOBST(IOBS) + 10
	
	! is the obstacle completed?
	IF ( TOBST(IOBS) .GE. 100) THEN
	  IAMDIGGING (IUNIT, ISIDE) = 0
	  TOBST(IOBS) = 100
	  MY_OBSTACLE(IUNIT, ISIDE) = 0
	ENDIF

 999    CONTINUE

        RETURN
        END

		 * 
		 */
	}

	/**
	 * Test to see if any obstacles block the entity's path.
	 * <p> TODO engineering not yet implemented.
	 */
	private void findObstacles(){ // blockmove from domove
		/*
		 * 
		 * will set objective to location of obstacle, set flags for iriv etc

        CALL BLOCKMOV ( IUNIT, ISIDE, X1,Y1, X2, Y2, IRIV, IOBS, MINES )

D	IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$, "(12X,'River, obs, mines ',3(x,i1))") IRIV, IOBS, MINES
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

C ----- If unit can't move, we are done.

	SUBROUTINE  BLOCKMOV ( IUNIT, ISIDE, X1,Y1, X2, Y2, IRIV, IOBS, IMINES )

C-----------------------------------------------------------------------C
C									C
C	PURPOSE:  To determine if a unit's movement is stopped by any	C
C		  obstacle or terrain feature.  Additionally, allow	C
C		  "engineer capable" units to clear obstacles.		C
C									C
C-----------------------------------------------------------------------C
C									C
C	GLOBMOVTMP Input Arguments:					C
C									C
C	X1,Y1	 -  Point unit is starting from.			C
C	X2,Y2	 -  Point unit is attempting to moving to		C
C	IBRETYP  -  Type of breaching unit (if in breach mode)		C
C									C
C-----------------------------------------------------------------------C
C									C
C	GLOBMOVTMP Output Arguments:					C
C									C
C	X2,Y2	-  Point where unit is stopped (if stopped prior	C
C		   to reaching its input destination point).		C
C									C
C	IENGTYP -  Engineer Type of unit.				C
C									C
C	DELAY	-  Delay time (minutes) required to clear or cross	C
C		   an obstacle or terrain feature in the unit's		C
C		   movement path (for this movement cycle only).	C
C									C
C	IGRIDWORD  -  Grid-cell word for point (X2,Y2).			C
C									C
C	MFLANE     -  If > 0, unit is in a cleared minefield lane.	C
C									C
C-----------------------------------------------------------------------C
C                                                                       C
C                                                                       C
C       Modification Log                                                C
C                                                                       C
C                                                                       C
C       SCR # 183  LOS                                6/21/94 - EVH     C
C       Changed call to WRITMOVE to use movement file write parameter   C
C       defined in GLBPARAM.                                            C
C                                                                       C
C       SCR # 311  Minefield kills                   10/31/94 - EVH     C
C       Changed parameter list on call to MINEVAL.                      C
C                                                                       C
C       SCR # 147  Building Rubbling                  8/25/96 - EVH     C
C       Prevent soldiers from entering rubbled buildings (building is   C
C       negative).  They can still cross it, but KINBILD is set to 0.   C
C       Prevent vehicles from crossing rubbled building.                C
C                                                                       C
C       SCR # 153  Dead Vehicle Barriers (Hulks)      5/22/96 - GLM     C
C       Replaced EDELAY(5,1) with AVLBDLAY variable.                    C
C                                                                       C
C       SCR # 154  Multiple Kill Catagories           2/17/97 - EVH     C
C       Add parent unit to WRITMOVE call.                               C
C                                                                       C
C       Port to PC                                    9/24/97 - EVH     C
C       Striped VMS logical from INCLUDE file name and made lower       C
C       case.                                                           C
C                                                                       C
C                                                                       C
C-----------------------------------------------------------------------C

	INCLUDE		'global.f'		! CLOCK, KNUMVIEWS
	INCLUDE		'globunits.f'		! ENGNEER,FLAST,SWIMMRS
	INCLUDE		'globmove.f'		! NMOVTOBLDG
	INCLUDE		'glbedlay.f'		! EDELAY
	INCLUDE		'globtrrn.f'		! KRIVERDLAY
	INCLUDE		'globrpt.f'

C ----- Init the unit's movement delay to zero.

	DELAY  =  0.0

	IANY = 0
	IOBS = 0
	IBILDS = 0
	IMINES = 0
	IRIV = 0
	IFENCS = 0
	ISTR = 0


	IGRIDWORD  =  ITRRNDATA( X1, Y1 )

c	IF ( IGRIDWORD .AND. MASKANY ) THEN
c	  IANY = 1
c	ENDIF

C ----- Check if grid has an obstacle within it.

	IF ( IGRIDWORD .AND. MASKOBS ) THEN
	  IOBS = 1
	ENDIF

C ----- CHECK FOR BUILDINGS

	IF ( IGRIDWORD .AND. MASKBILD ) THEN
	  IBILDS = 1
	ENDIF

C ----- CHECK FOR MINEFIELDS

	IF ( IGRIDWORD .AND. MASKMINE ) THEN
	  IMINES = 1
	ENDIF

C ----- CHECK IF UNIT PATH CROSSES A RIVER

	IF ( IGRIDWORD .AND. MASKRIVER ) THEN
	  IRIV = 1
	ENDIF

C ----- CHECK FOR FENCES

	IF ( IGRIDWORD .AND. MASKFENC ) THEN
	  IFENCS = 1
	ENDIF

C ----- CHECK FOR "GENERIC" STRINGS

	IF ( IGRIDWORD .AND. MASKSTR ) THEN
	  ISTR = 1
	ENDIF


	IGRIDWORD  =  ITRRNDATA( X2, Y2 )

c	IF ( IGRIDWORD .AND. MASKANY ) THEN
c	  IANY = 1
c	ENDIF

C ----- Check if grid has an obstacle within it.

	IF ( IGRIDWORD .AND. MASKOBS ) THEN
	  IOBS = 1
	ENDIF

C ----- CHECK FOR BUILDINGS

	IF ( IGRIDWORD .AND. MASKBILD ) THEN
	  IBILDS = 1
	ENDIF

C ----- CHECK FOR MINEFIELDS

	IF ( IGRIDWORD .AND. MASKMINE ) THEN
	  IMINES = 1
	ENDIF

C ----- CHECK IF UNIT PATH CROSSES A RIVER

	IF ( IGRIDWORD .AND. MASKRIVER ) THEN
	  IRIV = 1
	ENDIF

C ----- CHECK FOR FENCES

	IF ( IGRIDWORD .AND. MASKFENC ) THEN
	  IFENCS = 1
	ENDIF

C ----- CHECK FOR "GENERIC" STRINGS

	IF ( IGRIDWORD .AND. MASKSTR ) THEN
	  ISTR = 1
	ENDIF


	RETURN





	IF ( X1 .LT. X2 ) THEN
	  XSTART = X1
	  XEND = X2
	ELSE
	  XSTART = X2
	  XEND = X1
	ENDIF

	IF ( Y1 .LT. Y2 ) THEN
	  YSTART = Y1
	  YEND = Y2
	ELSE
	  YSTART = Y2
	  YEND = Y1
	ENDIF

	X = XSTART
	Y = YSTART

	DO WHILE ( X .LE. XEND )
	DO WHILE ( Y .LE. YEND )

C ----- Are there ANY terrain features present which could prevent unit
C       from moving to point (X2,Y2)?

	IGRIDWORD  =  ITRRNDATA( X, Y )

c	IF ( IGRIDWORD .AND. MASKANY ) THEN
c	  IANY = 1
c	ENDIF

C ----- Check if grid has an obstacle within it.

	IF ( IGRIDWORD .AND. MASKOBS ) THEN
	  IOBS = 1
	ENDIF

C ----- CHECK FOR BUILDINGS

	IF ( IGRIDWORD .AND. MASKBILD ) THEN
	  IBILDS = 1
	ENDIF

C ----- CHECK FOR MINEFIELDS

	IF ( IGRIDWORD .AND. MASKMINE ) THEN
	  IMINES = 1
	ENDIF

C ----- CHECK IF UNIT PATH CROSSES A RIVER

	IF ( IGRIDWORD .AND. MASKRIVER ) THEN
	  IRIV = 1
	ENDIF

C ----- CHECK FOR FENCES

	IF ( IGRIDWORD .AND. MASKFENC ) THEN
	  IFENCS = 1
	ENDIF

C ----- CHECK FOR "GENERIC" STRINGS

	IF ( IGRIDWORD .AND. MASKSTR ) THEN
	  ISTR = 1
	ENDIF

	Y = Y + (GRIDSIZE / 2)

	ENDDO

	Y = YSTART
	X = X + (GRIDSIZE / 2)

	ENDDO

C ----- RETURN to calling routine.

  999	CONTINUE

	RETURN
	END


*/
		
	}

	/**
	 * Do housekeeping if the entity is moving within or between buildings.
	 * <p> TODO buildings not yet implemented.
	 * @return True if stopped by a building.
	 */
	private boolean processUrbanTerrain(){ // from domove
		/*
	IBUILD1 = ON_ROOF(IUNIT, ISIDE)
	IF ( IBUILD1 .LE. 0 ) THEN
	  IBUILD1 = IN_BUILDING(IUNIT, ISIDE)
	ENDIF

	IBUILD2 = IBUILD1

	IDEBUG = 0
	IF ( DEBUGMOVE .AND. ITRACK(IUNIT,ISIDE) )THEN
	  IDEBUG = 1
	ENDIF

	CALL TRRN_IN_BUILDING  ( IBUILD2, X2,Y2, IDEBUG )

D	IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	  WRITE (KLINE$, "(12X,'The start point is inside building ',I6)") IBUILD1
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$, "(12X,'Trying to move into building ',I6)") IBUILD2
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF ( IBUILD1 .NE. IBUILD2 ) THEN

D	  IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	    WRITE (KLINE$, "(12X,'We are moving from one building to another ')")
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$, "(12X,'or into or out of a building ')")
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( IBUILD1 .GT. 0 ) THEN ! moving from a building
	    CALL TRRN_GET_BUILD_FLOORS(IBUILD1, IFLOORS)

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$, "(12X,'Start building has ',I3,' floors left')") IFLOORS
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF ( IFLOORS .GT. 0 ) THEN

	      IF ( (ON_FLOOR(IUNIT,ISIDE) .GT. 0) .OR. (ON_ROOF(IUNIT,ISIDE) .GT. 0 ) ) THEN

D	        IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$, "(12X,'Can only enter/ exit buildings via the ground floor')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	        CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	        CALL LINEOUT (1,0,0)
	        WRITE (KLINE$,"('Stopped on upper floor')")
	        CALL LINEOUT (0,0,0)
	        CALL LINEOUT (2,0,0)

	        X2 = X1
	        Y2 = Y1
	        CALL STOPUNIT(IUNIT, ISIDE, X1, Y1, 1, 0)
	        GOTO 999
	      ELSE ! we must be on the ground floor
	        IF ( IMOVER .NE. MOVFOOT )  THEN

D	          IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	            WRITE (KLINE$, "(12X,'Non-foot cant exit buildings')")
D	            CALL TRACEOUT (0,0,0)
D	          ENDIF

	          CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	          CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	          CALL LINEOUT (1,0,0)
	          WRITE (KLINE$,"('Stopped by a building')")
	          CALL LINEOUT (0,0,0)
	          CALL LINEOUT (2,0,0)

	          X2 = X1
	          Y2 = Y1
	          CALL STOPUNIT(IUNIT, ISIDE, X1, Y1, 1, 0)
	          GOTO 999
	        ENDIF
	      ENDIF
	    ELSE ! no floors left, anything can move over the rubble
	    ENDIF
	  ENDIF

	  IF ( IBUILD2 .GT. 0 ) THEN ! moving to a building
	    CALL TRRN_GET_BUILD_FLOORS(IBUILD2, IFLOORS)

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$, "(12X,'Final building has ',I3,' floors left')") IFLOORS
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    IF ( IFLOORS .GT. 0 ) THEN
	      IF ( IMOVER .EQ. MOVFOOT )  THEN

D	        IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$, "(12X,'Foot can enter or leave buildings')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        DELAY = 3.0 / 60.0
	        IF ( IBUILD1 .GT. 0 ) THEN
	          DELAY = DELAY * 2.0
	        ENDIF

	        CALL SETDLAY ( IUNIT, ISIDE, DELAY*60)
	        DX  =  X2 - X1
	        DY  =  Y2 - Y1
	        DISTMOVD  =  SQRT( DX*DX + DY*DY )

	        IN_BUILDING(IUNIT,ISIDE) = IBUILD2
	        ON_FLOOR(IUNIT,ISIDE) = 0 ! on ground floor
	        AT_WALL(IUNIT,ISIDE) = 0
	        ON_ROOF(IUNIT,ISIDE) = 0
	        GOTO 999

	      ELSE

D	        IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	          WRITE (KLINE$, "(12X,'Non-foot cant enter buildings')")
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	        CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	        CALL LINEOUT (1,0,0)
	        WRITE (KLINE$,"('Stopped by a building')")
	        CALL LINEOUT (0,0,0)
	        CALL LINEOUT (2,0,0)

	        X2 = X1
	        Y2 = Y1
	        CALL STOPUNIT(IUNIT, ISIDE, X1, Y1, 1, 0)
	        GOTO 999
	      ENDIF
	    ELSE ! building has no floors left
	      IN_BUILDING(IUNIT,ISIDE) = 0
	      ON_FLOOR(IUNIT,ISIDE) = 0
	      AT_WALL(IUNIT,ISIDE) = 0
	      ON_ROOF(IUNIT,ISIDE) = IBUILD2
	    ENDIF
	  ELSE ! not entering a building
	    IN_BUILDING(IUNIT,ISIDE) = 0
	    ON_FLOOR(IUNIT,ISIDE) = 0
	    AT_WALL(IUNIT,ISIDE) = 0
	    ON_ROOF(IUNIT,ISIDE) = 0
	  ENDIF

	ENDIF

		 * 
		 */
		return false;
	}
	
	/**
	 * Update the minefield lane this entity is creating.
	 * <p> TODO engineering not yet implemented.
	 * @param c1
	 * @param c2
	 */
	private void drawLane(Coordinate c1, Coordinate c2){
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,3,"drawing mine lane from " + c1.toString());
			Tracer.write(Tracer.MOVEMENT,3,"                  to   " + c2.toString());
		}
		/*
		  IF( IBREACH_MINES(IUNIT,ISIDE) .GT. 0 ) THEN
		    CALL DRAWLANE ( X1,Y1, X2,Y2, IUNIT, ISIDE )
		  ENDIF
		 */		
	}
	
	
	/**
	 * Test to see if the entity is slowed down by smoke.
	 * <p> TODO smoke not yet implemented.
	 * @param speed
	 * @return
	 */
	private double checkSmoke(double speed){
		return speed;
		/*
C------ If non-footed unit is in smoke (or dust) cloud, degrade its speed

	IF( MOVERS(ITYPE) .NE. MOVFOOT )  THEN
	  X1  =  XUNIT(IUNIT,ISIDE)
	  Y1  =  YUNIT(IUNIT,ISIDE)
	  CALL INLASMOK ( X1,Y1, 2.0, ISEE )
	  IF( ISEE .EQ. 1 )  THEN
	    ISEE  =  IUNIT
	    CALL INSMOK ( X1,Y1, 2.0, ISEE )
	  ENDIF
	  IF( ISEE .NE. 1 )  THEN

C-------------- Unit is inside a cloud

	    IF( ISLOW(IUNIT,ISIDE) .LT. 0 )  THEN
	      SPDFACTR  =  ISLOW(IUNIT,ISIDE)
	      SPDFACTR  =  - SPDFACTR * 0.01
	    ELSE
	      CALL UNIRAN (DRAW)
	      SPDFACTR  =  0.25 + ( 0.5 * DRAW )
	      SPEEDTMP  =  -SPDFACTR * 100.0
	      ISLOW(IUNIT,ISIDE)  =  SPEEDTMP
	    ENDIF
	    SPEED  =  SPEED * SPDFACTR

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"(12X,'Icon is within a smoke cloud, speed = ',F10.3)") SPEED
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ELSE

D	    IF (DEBUGMOVE .GT. 0 .AND. ITRACK(IUNIT,ISIDE) .GT. 0 ) THEN
D	      WRITE (KLINE$,"(12X,'Not in a smoke cloud')")
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	  ENDIF
	ENDIF
			 * 
		 * 
		 */
	}

	/**
	 * Determine if a bridge needs to be recovered or deployed.
	 * <p> TODO bridging not yet implemented.
	 * @param platform
	 * @param currentLocation
	 * @return
	 */
	private boolean waitingForMyBridge(Platform platform, Coordinate currentLocation){ 
		// from domove
		BridgeLayer bridgeLayer = platform.getBridgeLayer();
		if ( bridgeLayer == null){
			if (tracing){
				Tracer.write(Tracer.MOVEMENT,4,"not a bridge layer ");
			}
			return false;
		}
		if (tracing){
			Tracer.write(Tracer.MOVEMENT,4,"i am a bridge layer");
			Tracer.write(Tracer.MOVEMENT,4,"my bridge is " +
					Constants.BRIDGE_STATE[this.myEntity.getBridgeState()]);
		}
		// can only have got to here if not delayed
		
		if ( this.myEntity.getBridgeState() == Constants.BRIDGE_RECOVERING ||
				this.myEntity.getBridgeState() == Constants.BRIDGE_RECOVERED){
			if (this.myEntity.recoverBridge(this.nextMoveTime)){
				if (tracing){
					Tracer.write(Tracer.MOVEMENT,4,"bridge is recovered");
				}
				return false;
			}
		}
		/*
		 * 
	IF ( BRIDGE_LAYER(ITYPE) .GT. 0 ) THEN

D	  IF ( (DEBUGMOVE .OR. DEBUGENGR) .AND. ITRACK(IUNIT,ISIDE) )THEN
D	    write (KLINE$,"('I am a bridge')")
D	    CALL TRACEOUT (0,0,0)
D	    write (KLINE$,"('State ',I2)") IAVLBDEP(IUNIT,ISIDE)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  IF ( IAVLBDEP (IUNIT,ISIDE) .NE. IBRIDGE_RECOVERED ) THEN ! With bridge deployed

	    IF ( IAVLBDEP (IUNIT,ISIDE) .EQ. IBRIDGE_RECOVERING ) THEN ! must have finished delay time, so completed recovery
	      IAVLBDEP(IUNIT,ISIDE) = IBRIDGE_RECOVERED

D	      IF ( (DEBUGMOVE .OR. DEBUGENGR) .AND. ITRACK(IUNIT,ISIDE) )THEN
D	        CALL ICON_ID(IUNIT, ISIDE, KLINE$)
D	        CALL TRACEOUT (0,2,0)
D	        write (KLINE$,"('Bridge recovered')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	      CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	      CALL LINEOUT (1,0,0)
	      WRITE (KLINE$,"('Bridge recovered')")
	      CALL LINEOUT (0,0,0)
	      CALL LINEOUT (2,0,0)

	      GOTO 100
	    ENDIF

	    IF (BRIDGE_ACTION(IUNIT,ISIDE)  .NE. 0 ) THEN ! Bridge which is
						! no longer breaching is able
						! to move.
	      IAVLBDEP(IUNIT,ISIDE) = IBRIDGE_RECOVERING
	      IBRIDGE = BRIDGE_LAYER(ITYPE)

C	      DELAY  =  OBST_BR_TIME(IOBST_ANTI_TANK,ENGTYPE_AVLB)	! delay while bridge is picked up
	      DELAY = BRIDGE_PACKUP_TIME(IBRIDGE)

D	      IF ( (DEBUGMOVE .OR. DEBUGENGR) .AND. ITRACK(IUNIT,ISIDE) )THEN
D	        CALL ICON_ID(IUNIT, ISIDE, KLINE$)
D	        CALL TRACEOUT (0,2,0)
D	        write (KLINE$,"('Entity recovering bridge ')")
D	        CALL TRACEOUT (0,0,0)
D	        write (KLINE$,"('Delayed for ',F6.3,' mins')") DELAY
D	        CALL TRACEOUT (0,0,2)
D	      ENDIF

	      CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	      CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	      CALL LINEOUT (1,0,0)
	      WRITE (KLINE$,"('Entity recovering bridge ')")
	      CALL LINEOUT (0,0,0)
	      WRITE (KLINE$,"('Delayed for ',F6.3,' mins')") DELAY
	      CALL LINEOUT (0,0,0)
	      CALL LINEOUT (2,0,0)

	      CALL SETDLAY ( IUNIT, ISIDE,  DELAY*60)
	      DX  =  X2 - X1
	      DY  =  Y2 - Y1
	      DISTMOVD  =  SQRT( DX*DX + DY*DY )

	      IF( DISTMOVD .GT. 0.03 )  THEN
	        RATIO  =  0.03 / DISTMOVD
	        DX  =  DX * RATIO
	        DY  =  DY * RATIO
	        DISTMOVD  =  0.03
	        X2  =  X1 + DX
	        Y2  =  Y1 + DY
	      ENDIF
	      GOTO 999
	    ELSE	! AVLB is still breaching and unable to move
			! this is triggered because the avlb was delayed, but at a go node
			! until the bridge is actually deployed.
			! after that, the go node is stopped.
	      CALL SET_UNIT_WINDOW( 1, IUNIT, ISIDE, LUN )
	      CALL ICON_ID (IUNIT, ISIDE, KLINE$)
	      CALL LINEOUT (1,0,0)
	      WRITE (KLINE$,"('Entity has completed deploying its bridge')")
	      CALL LINEOUT (0,0,0)

	      X2 = X1
	      Y2 = Y1
	      CALL STOPUNIT(IUNIT, ISIDE, X1, Y1, 1, 0)
	      NTIME = IAVLBDEP_TIME(IUNIT,ISIDE)
	      IAVLBDEP(IUNIT, ISIDE) = IBRIDGE_DEPLOYED

D	      IF ( (DEBUGMOVE .OR. DEBUGENGR) .AND. ITRACK(IUNIT,ISIDE) )THEN
D	        write (KLINE$,"('Bridge deployed')")
D	        CALL TRACEOUT (0,0,0)
D	      ENDIF

	      IF ( NTIME > 0 ) THEN
	        NTIME = NTIME + CLOCK*60.0
	        CALL HOLDUNIT(IUNIT,ISIDE, NTIME)
	        BRIDGE_ACTION(IUNIT,ISIDE) = IBRIDGE_RECOVER

	        TIME = NTIME
	        TIME = TIME / 60
	        TIMESTRING$ = '00:00:00:00'
	        CALL CONV_CLOCK( TIME, TIMESTRING$ )

D	        IF ( (DEBUGMOVE .OR. DEBUGENGR) .AND. ITRACK(IUNIT,ISIDE) )THEN
D	          write (KLINE$,"('Bridge holding until ',A11)") TIMESTRING$
D	          CALL TRACEOUT (0,0,0)
D	        ENDIF

	        write (KLINE$,"('Bridge will recover at ',A11)") TIMESTRING$
	        CALL LINEOUT (0,0,0)
	      ENDIF

	      CALL LINEOUT (2,0,0)

	      GOTO 999
	    ENDIF
	  ENDIF
	ENDIF
		 */
		return false;
	}
	
}
