package sim;

import java.time.Instant;
import java.util.Vector;

import data.CSData;
import interpreter.CommandInterpreter;
import models.ClockUpdateEvent;
import models.EventFactory;
import models.EventQueue;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;
import utils.Tracer;
import view.ClockListener;

public class Simulation {
	
	/*
	 **************************************
	 * Class attributes
	 **************************************
	 */
	
	private boolean planning = false; // flag in planning mode
	private boolean fighting = false; // flag in battle mode
	private boolean threadRunning = false; // flag sim thread is running
	
	private Scenario myScenario;
	private GameClock gameClock;
	private CSData myData;
	private EventQueue eventQueue;

	private boolean autoStart = false; // should sim go straight to battle mode
	private Runnable simThread;

	/*
	 **************************************
	 * Main
	 **************************************
	 */
	
	/**
	 * This class is executable. Running it runs a test.
	 * @param configuration arguments that may be used to configure the test. 
	 */
	public static void main(String[] configuration){
		// example command line args
		// --load tests/test_scan_event.txt
		//--load tests/test_sim_01_default_entity_events.txt
		
		Logger.say("running Simulation.main");
		Simulation sim = new Simulation();
		// add a clock listener to report the time for the test
		sim.addClockListener(new ClockListener(){
			public void updateClock(double clock) {
				Logger.log("time " + Parser.formatTime(clock));
			}
		});
		
		// if there are no arguments, set some defaults to make the test run quickly
		if (configuration.length<=0){
			configuration = defaultConfiguration();
		}
		sim.initialise(configuration);
	}
	
	private static String[] defaultConfiguration(){ 
		String temp[] = {
				"--scenario", 
				"start_time", 
				"0:00", 
				"--scenario", 
				"end_time", 
				"10:00", 
				"--scenario", 
				"time_step",
				"1:00",
				"--scenario",
				"real_time",
				"no"
				};
		return temp;
	}
	
	/*
	 **************************************
	 * constructors
	 **************************************
	 */
	
	/**
	 * Create a simulation without a controller.
	 */
	public Simulation(){
		autoStart = true;
	}
	
	/**
	 * Create a simulation with a controller.
	 * @param start True if the simulation should automatically
	 * go straight from planning mode to running.
	 */
	public Simulation (boolean start){
		autoStart = start;
	}
	
	/*
	 **************************************
	 * public methods
	 **************************************
	 */
	
	public void initialise(String[] args){
		if (threadRunning) return;
		Logger.log("initialising data ");
		
		myScenario = new Scenario("simulation_test");
		gameClock = new GameClock(myScenario);
		myData = new CSData();
		myInterpreter = new CommandInterpreter(this);
		eventQueue = new EventQueue();
		interpret(args);
		
		setup();

		deployMode();
	}
	
	public void battle(){
		if (!planning ) return;
		if (fighting) return;
		battleMode();
	}

	public void end(){
		updateClockListeners();
		Logger.log("simulation ended at " + gameClock.toString());
		Logger.log("current time " + Instant.now().toString());
	}
	
	/*
	 **************************************
	 * private methods
	 **************************************
	 */
	
	/*
	 * do setup stuff prior to initial planning like relocate
	 */
	private void setup(){
		relocateEntities();
		//relocatePrepos(); // see relocate.f
	}
	
	private void relocateEntities(){
		//make sure all entities are on the map.
		for (String entityName : getScenario().getEntityList().keySet()){
			Entity entity = getScenario().getEntityList().getEntity(entityName);
			if (!this.myScenario.getMap().onMap(entity.getLocation())){
				relocateEntity(entity);
			}
		}
	}
	
	private void relocateEntity(Entity entity){
		// TODO
		entity.setLocation(myScenario.getMap().getRandom());
		/*
		 * 
		ITASK  =  KTASKFOR(IUNIT,ISIDE)
		ICOUNT(ITASK,ISIDE)  =  ICOUNT(ITASK,ISIDE) + 1
		ICOL  =  MOD( ICOUNT(ITASK,ISIDE) - 1, 80 )
		IROW  =     ( ICOUNT(ITASK,ISIDE) - 1 ) / 80
		ITASK  =  MOD( ITASK-1, NGPSPERFRC )  + 1
	        X = XMIN + XDIST * (ICOL +10)
	        Y = YMAX - XDIST * (ITASK *8) - XDIST * IROW
		XUNIT(IUNIT,ISIDE)  =  X
		YUNIT(IUNIT,ISIDE)  =  Y
*/
		Logger.log("entity " + entity.getName() + 
				" relocated to " + entity.getLocation().toString());
		entity.reset_icon();
		entity.clrmsns();
		entity.clrnodes();
/*
	        IN_BUILDING(IUNIT, ISIDE) = 0 ! set not in a building
	        ON_FLOOR(IUNIT,ISIDE) = 0
	        AT_WALL(IUNIT,ISIDE) = 0
	        ON_ROOF(IUNIT,ISIDE) = 0
		  DO  IPASS = 1, NUMUNITS
		    IF( MOUNTED(IPASS,ISIDE) .EQ. IUNIT )  THEN
			CALL CLRNODES ( IPASS, ISIDE )
			CALL CLRMSNS  ( IPASS, ISIDE )
		    ENDIF
		  ENDDO
		 */
	}
	
	private void deployMode(){
		Logger.say("scenario name " + this.myScenario.getName());
		
		planning = true;

		simThread = new RunSim();
		Thread thread = new Thread(simThread);
		thread.start();

		Logger.log("deploy mode " + getScenario().getName());
		Logger.log("current time " + Instant.now().toString());

		if ( autoStart){
			Logger.log("automatically entering battle mode ");
			battleMode();
		}
	}

	private void battleMode(){
		if (fighting) return;
		planning = false;
		Logger.log("battle mode " + getScenario().getName());
		Logger.log("current time " + Instant.now().toString());
		Logger.log("starting simulation " + getScenario().getName());
		Logger.log("start time " + Parser.formatTime(myScenario.getParameters().getStartTime()));
		Logger.log("end time   " + Parser.formatTime(myScenario.getParameters().getEndTime()));
		gameClock.setClockSecs(myScenario.getParameters().getStartTime());
		updateClockListeners();
		initQueue();
		
		fighting = true;
		gameClock.setSynchPoint();
	}
	
	private void initQueue(){ //TODO add all the extra event types
		Logger.log("initialising queues");
		eventQueue.add(EventFactory.makeClockUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		eventQueue.add(EventFactory.makeCloudUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		eventQueue.add(EventFactory.makeChemicalUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		eventQueue.add(EventFactory.makeStatusUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		eventQueue.add(EventFactory.makeSaveEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		for (String entityName : getScenario().getEntityList().keySet()){
			Entity entity = getScenario().getEntityList().getEntity(entityName);
			eventQueue.add(EventFactory.makeMoveEvent(entity, this));
			eventQueue.add(EventFactory.makeDetectEvent(entity, this));
			eventQueue.add(EventFactory.makeScanEvent(entity, this));
			eventQueue.add(EventFactory.makeShootEvent(entity, this));
			eventQueue.add(EventFactory.makeSuppressionEvent(entity, this));
			eventQueue.add(EventFactory.makeResupplyEvent(entity, this));
			eventQueue.add(EventFactory.makeCasualtyEvent(entity, this));
			eventQueue.add(EventFactory.makeDetectObstacleEvent(entity, this));
			
			// firing?, impact, active sensors/ radar, doarty
		}
	}
	
	private void doUserEvents(){
	}
	
	private void doSimEvents(){
		
		// determine the time to advance the clock 
		double nextTime = eventQueue.nextTime();
		if (nextTime >= myScenario.getParameters().getEndTime()){
			nextTime = myScenario.getParameters().getEndTime();
		}

		// advance the clock
		gameClock.updateClockSecs(nextTime);
		
		// do any events due at this time
		eventQueue.doEvents(gameClock.getClockSecs());
		
		// if we have reached the end of the simulation, signal a stop
		if (gameClock.getClockSecs() >= myScenario.getParameters().getEndTime()){
			threadRunning = false;
		}
	}

	/**
	 * Define an interpreter create a facade
	 */
	private CommandInterpreter myInterpreter;
	public void interpret(String command){
		myInterpreter.interpret(command);
	}
	public void interpret(String[] command){
		myInterpreter.interpret(command);
	}
	public void interpret(Vector<String> command){
		myInterpreter.interpret(command);
	}
	
	public Scenario getScenario(){return myScenario;} 
	public CSData getCSData(){return myData;} 
	public GameClock getGameClock(){return gameClock;} 
	
	private Vector<ClockListener> clockListeners = new Vector<ClockListener>();
	public void addClockListener(ClockListener listener){
		clockListeners.addElement(listener);
	}
	public void updateClockListeners(){ 
		for (ClockListener listener : clockListeners){
			double clock = this.gameClock.getClockSecs();
			listener.updateClock(clock);
		}
	}

	
	/*
	 * run the simulation in a thread
	 */

	class RunSim implements Runnable{
		@Override
		public void run() {
			if (myScenario == null) return;
			if (eventQueue == null) return;
			threadRunning = true;
			while ( threadRunning){
				doUserEvents();
				if (fighting){
					doSimEvents();
				}
			}
			end();
		}
	}
	
}
