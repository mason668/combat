package sim.entity;

import java.util.Vector;

import data.csd.Platform;
import data.map.Building;
import data.map.Coordinate;
import sim.obstacles.Pit;
import sim.FirePort;
import sim.obstacles.Obstacle;
import sim.route.Route;

/**
 * A facade for the Entity class that provides all of the methods
 * needed for an entity to move. 
 */
public interface MoverEntity {
	

	/**
	 * Request The entity notify the user of an event.
	 * @param message The message to send to the user.
	 */
	public void alert(String message);

	/**
	 * Determine if the entity is at a fire port in a building.
	 * @return The fire port or null.
	 */
	public FirePort atFirePort();

	/** 
	 * Clear the circle order for this entity.
	 * 
	 */
	public void clearCircle();

	/**
	 * If the entity is at a fire port, exit it and clear the fire port.
	 */
	public void clearFirePort();
	
	/**
	 * Clears the order for an entity to move to a floor in a building. 
	 */
	public void clearGotoFloor();

	/**
	 * Causes an entity to consume an amount of fuel.
	 * @param amount The amount of fuel to consume.
	 */
	public void consumeFuel(double amount);

	/**
	 * Cause an entity to enter a vehicle deployed bridge.
	 * @param bridge The bridge to enter.
	 * @return True if the action was successful.
	 */
	public boolean enterBridge(BridgeEntity bridge);

	/**
	 * If the entity is in a pit, exit it.
	 */
	public void exitPit();
	
	/**
	 * Determine if an entity has fired recently.
	 * @param time The current game time.
	 * @return True if the entity has fired recently. 
	 */
	public boolean firedRecently(double time);

	/**
	 * Get the height of the entity off the ground for ground movers.
	 * @return Entity height.
	 */
	public double getBaseHeight();

	/**
	 * If this entity is capable of deploying a bridge, determine the state of the bridge.
	 * @see sim.Constants
	 * @return The state of the bridge.
	 */
	public int getBridgeState();

	/**
	 * Get the building occupied by this entity.
	 * @return The building object or null if the entity is not in a building.
	 */
	public Building getBuilding();

	/**
	 * Get the status of any CBR carried by this entity.
	 * @return The CBR status as an integer.
	 * @see Sim.Constants
	 */
	public int getCBRStatus();

	/**
	 * Get the time this entity will complete its setup or packup of its CBR
	 * @return The game time the operation will be completed.
	 */
	public double getCBRTime();
	
	/**
	 * If this entity is currently circling, get the coordinate for the 
	 * centre of the circle.
	 * @return The coordinate of the centre, or null if not circling.
	 */
	public Coordinate getCircle();
	
	/**
	 * Identify if the entity is circling clockwise or anticlockwise.
	 * @return True if circling clockwise, false if not.
	 */
	public boolean getCircleClockwise();
	
	/**
	 * Identify if the entity has been ordered to mover toward and 
	 * mount another entity, as a passenger.
	 * @return The target carrier or null if there is no mount order.
	 */
	public CarrierEntity getCommandMount();

	/**
	 * Identify if the entity has been ordered to move toward and enter a pit.
	 * @return The target pit or null if there is no pit order.
	 */
	public Pit getCommandPit();

	/**
	 * Get the speed this entity has been ordered to use.
	 * @return The entities ordered speed (kph).
	 */
	public double getCommandSpeed();

	/**
	 * Identify if this entity is part of a convoy.
	 * @return The entity that is leading the convoy or null if this entity is 
	 * not part of a convoy.
	 */
	public MoverEntity getConvoyLeader();

	/**
	 * Get the current amount of fuel available for this entity.
	 * @return The volume of fuel available.
	 */
	public double getCurrentFuel();

	/**
	 * Get the actual speed this entity is currently traveling at.
	 * @return The entity's speed (kph).
	 */
	public double getCurrentSpeed();

	/**
	 * Get the current defilade state for this entity.
	 * @return The defilade state.
	 * @see Sim.Constants
	 */
	public int getDefilade();

	/**
	 * Get the current delay time for this entity.
	 * @return The delay time (secs).
	 */
	public double getDelay();

	/**
	 * Get the direction this entity is currently facing.
	 * @return The direction in radians from east.
	 */
	public double getDirectionFace();

	/**
	 * Get the direction this entity is currently moving toward.
	 * @return The direction in radians from east.
	 */
	public double getDirectionMove();

	/**
	 * Get the direction this entity is currently viewing.
	 * @return The direction in radians from east.
	 */
	public double getDirectionView();

	/**
	 * If this entity is in a building, get the floor it is on.
	 * @return The floor number.
	 */
	public int getFloor();

	/**
	 * Identify if this entity is part of a formation and if so, get the leader entity.
	 * @return The entity leading the formation or null if this entity is not part 
	 * of a formation.
	 */
	public MoverEntity getFormationLeader();

	/**
	 * If this entity has been ordered to change floors, get the floor it has 
	 * been ordered to move to.
	 * @return The floor number (zero = ground).
	 */
	public int getGotoFloor();

	/**
	 * If this entity is at a hold node, get the time it elapses.
	 * @return The hold time in seconds, or zero if it is not holding.
	 */
	public double getHoldTime();

	/**
	 * Get the expanded ID of this entity.
	 * @return A string containing the expanded ID for the entity.
	 */
	public String getID();

	/** 
	 * Get the time this entity last moved.
	 * @return The time (secs) or -NEVER if the entity has not moved.
	 */
	public double getLastMoved();

	/**
	 * Get the current location for this entity.
	 * @return The entity's current location.
	 */
	public Coordinate getLocation();

	/**
	 * Get the current mine clearing mode for this entity.
	 * @return A mine clearing mode.
	 * @see sim.Constants
	 */
	public int getMineClearingMode();

	/**
	 * Get the current movement mode for this entity.
	 * @return The movement mode.
	 * @see sim.Constants
	 */
	public int getMoveMode();

	/** 
	 * identify of this entity is inoperative due to BNC effects.
	 * @return >0 if the entity is inoperative. Return code is the reason.
	 */
	public int getNBCInoperative();

	/**
	 * Get the number of elements in this entity (if an aggregate) that have
	 * mobility kills.
	 * @return The number of mobility kills.
	 */
	public int getNonMovers();

	/**
	 * Get the current number of living elements in this entity (if an aggregate).
	 * @return The number of elements.
	 */
	public int getNumberOfElements();

	/**
	 * Identify if this entity is in a pit.
	 * @return The pit if the entity occupies one, or null.
	 */
	public Pit getPit();

	/** 
	 * Get the platform type for this entity.
	 * @return The platform.
	 */
	public Platform getPlatform();

	/**
	 * Identify if this entity is on (or above for a flier) the roof of a building.
	 * @return The building if the entity is on the roof, or null.
	 */
	public Building getRoof();

	/**
	 * Identify if the entity has a valid movement route.
	 * @return The route if it has one, otherwise null.
	 */
	public Route getRoute();

	/**
	 * Get the time at which the entity will complete an upload activity.
	 * @return The time (secs) when the upload will complete.
	 */
	public double getUploadTime();

	/**
	 * Get the list of weapons carried by the entity.
	 * @return The list of weapons.
	 */
	public Vector<EntityWeapon> getWeaponList();

	/**
	 * Get the speed degradation factor if the entity must slow to fire a weapon.
	 * @return The speed factor (0<=f<=1).
	 */
	public double getSlowToFire();

	/**
	 * Identify if the entity is currently wearing NBC protection. 
	 * @return True if set with NBC protection.
	 */
	public boolean getMOPP();

	/**
	 * Get the radius if the circle for an entity that is circling.
	 * @return The radius (km).
	 */
	public double getCircleRadius();

	/**
	 * If an entity has a circle order, get the time the order will be completed.
	 * @return The time (secs).
	 */
	public double getCircleTime();

	/**
	 * If this entity is constructing an obstacle, get the obstacle.
	 * @return The obstacle or null.
	 */
	public Obstacle getConstructingObstacle();

	/**
	 * Get the location the entity is moving to if it has a move order.
	 * @return The movement objective if it has one, otherwise null.
	 */
	public Coordinate getMoveTo();

	/** 
	 * Get the entity that is carrying this entity if it mounted.
	 * @return The carrier entity if mounted, otherwise null.
	 */
	public CarrierEntity getCarrier();

	/**
	 * Get the flight mode (AGL or AMSL) for this entity.
	 * @return The flight mode.
	 * @see sim.Constants
	 */
	public int getFlightMode();

	/**
	 * Get the altitude for this entity (if it is a flier).
	 * @return The altitude (km).
	 */
	public double getAltitude();

	/**
	 * Triggers an entity at a stop or hold movement node to start moving.
	 */
	public void go();

	/**
	 * Order the entity to go down one floor in a building.
	 */
	public void goDown();

	/**
	 * Order the entity to go up one floor in a building.
	 */
	public void goUp();

	/**
	 * Test is the entity is using one of the move modes that represents crawling.
	 * @return true if crawling.
	 * @see sim.Constants
	 */
	public boolean iAmCrawling();

	/**
	 * Test if the entity is currently digging an obstacle.
	 * @return True if digging.
	 */
	public boolean iamDigging();

	/**
	 * Test if the entity is using one of the move modes that represents running.
	 * @return True if running.
	 */
	public boolean iAmRunning();

	/**
	 * Test if the entity is currently clearing mines.
	 * @return True if clearing mines.
	 */
	public boolean isClearingMines();

	/**
	 * Test if the entity is using a conical view instead of the standard view.
	 * @return True if using conical view.
	 */
	public boolean isConicalView();

	/**
	 * Test if the entity has its engine on.
	 * @return True if the engine is on.
	 */
	public boolean isEngineOn();

	/**
	 * Test if the entity is currently holding: waiting for time to elapse before moving. 
	 * @return True if holding.
	 */
	public boolean isHolding();

	/**
	 * Test if the entity is currently mounted.
	 * @return true if mounted.
	 */
	public boolean isMounted();

	/**
	 * Test if the entity is currently at a stop node.
	 * @return True if stopped.
	 */
	public boolean isStopped();

	/**
	 * Test if the entity is currently suppressed.
	 * @return true if suppressed.
	 */
	public boolean isSuppressed();

	/**
	 * Test if tracing has been requested for this entity.
	 * @return True if tracing is active.
	 */
	public boolean isTracing();

	/**
	 * Test if this entity is currently uploading, given the current clock. 
	 * @param clock The current game time.
	 * @return True if the entity is still uploading.
	 */
	public boolean isUploading(double clock);

	/**
	 * Order the entity to mount a carrier that is already nearby.
	 * @param carrier The carrier to mount.
	 */
	public void mount(CarrierEntity carrier);

	/**
	 * Order the entity to move to and occupy a pit.
	 * @param pit The pit to occupy.
	 */
	public void occupyPit(Pit pit);

	/**
	 * Test if the entity is currently on a bridge.
	 * @return The bridge, or null if not on a bridge.
	 */
	public BridgeEntity onBridge();

	/**
	 * Order an entity to recover a bridge.
	 * @param clock The current game time.
	 * @return True if the bridge is already recovered (or the entity is not a bridge layer).
	 */
	public boolean recoverBridge(double clock);

	/**
	 * Reset the fuel warning flag for an entity.
	 */
	public void resetFuelWarning();

	/**
	 * Order the entity to start or stop clearing mines.
	 * @param b If true, the entity will start clearing mines, otherwise it will stop.
	 */
	public void setClearMines(boolean b);

	/**
	 * Order the entity to move toward and mount another entity.
	 * @param carrier The entity to mount.
	 */
	public void setCommandMount(CarrierEntity carrier);

	/**
	 * Order the entity to construct an obstacle.
	 * @param obstacle The obstacle to construct.
	 */
	public void setConstructingObstacle(Obstacle obstacle);

	/**
	 * Order the entity to travel at a specific speed.
	 * @param speed The speed to use (kph).
	 */
	public void setCurrentSpeed (double speed);

	/**
	 * Set the defilade state for the entity.
	 * @param defiladeState The defilade state.
	 * @see sim.Constants
	 */
	public void setDefilade(int defiladeState);

	/**
	 * Set a delay for this entity.
	 * @param clock The time at which the delay elapses.
	 */
	public void setDelay(double clock);

	/**
	 * Set the direction of movement.
	 * @param direction Direction in radians from east.
	 */
	public void setDirectionMove(double direction);

	/**
	 * Turn the entity's engine off.
	 */
	public void setEngineOff();

	/**
	 * Turn the entity's engine on.
	 */
	public void setEngineOn();

	/**
	 * Record the time at which the entity last moved.
	 * @param time The time the entity moved (seconds).
	 */
	public void setLastMoved(double time);

	/**
	 * Update the entity's location. 
	 * @param coordinate The new location.
	 */
	public void setLocation(Coordinate coordinate);

	/**
	 * Set the location the entity has been ordered to move to.
	 * @param coordinate The location to move to. If null, the entity will cancel its 
	 * command move order.
	 */
	public void setMoveTo(Coordinate coordinate);

	/**
	 * Set the roof that the entity is on or above (if an aircraft).
	 * @param building
	 */
	public void setRoof(Building building);

	/**
	 * Order the entity to stop moving.
	 */
	public void stop();

	/**
	 * Order the entity to stop moving and send a message to the user explaining why.
	 * @param message The message to send.
	 */
	public void stop (String message);

	/**
	 * Force the entity to recalculate its conical view settings.
	 */
	public void updateConicalView();

	/**
	 * Update the entity's direction of view relative to its current direction of movement.
	 */
	public void updateDirectionView();

	/**
	 * Update the entity's direction of facing relative to its current direction of movement.
	 */
	public void updateDirectionFace();

	/**
	 * Force the entity to recalculate its mounted status.
	 */
	public void updateMount();
}
