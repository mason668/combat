package sim;

import java.time.Instant;
import java.util.Vector;

import data.CSData;
import interpreter.CommandInterpreter;
import models.ChemicalUpdateEvent;
import models.ClockUpdateEvent;
import models.CloudUpdateEvent;
import models.DetectEvent;
import models.EventFactory;
import models.EventQueue;
import models.MoveEvent;
import models.ScanEvent;
import models.ShootEvent;
import models.StatusUpdateEvent;
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
	
	private Scenario myScenario = new Scenario("simulation_test");
	private GameClock gameClock = new GameClock(myScenario);
	private CSData myData = new CSData();
	private CommandInterpreter myInterpreter = new CommandInterpreter(this);
	private boolean planning = false;
	private boolean battleMode = false;
	private boolean running = false;
	private EventQueue eventQueue = new EventQueue();
	private SimulationController simulationControl = null;
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
		Logger.say("running Simulation.main");
		Tracer.setEcho(true); //TODO temp so we get traces
		Tracer.setTraceLevel(Tracer.FINEST); //TODO we need this in the interpreter
		Simulation sim = new Simulation();
		sim.getScenario().getParameters().setRealTimeSynch(false);
		sim.getScenario().getParameters().setClockUpdateTime(24.0*60.0*60.0*10.0);
		//sim.getInterpreter().setTrace(true); //TODO not required
		// if there are no arguments, set some defaults to make the test run quickly
		
		// add a clock listener to report the time for the test
		sim.addClockListener(new ClockListener(){
			public void updateClock(double clock) {
				Logger.log("time " + Parser.formatTime(clock));
			}
		});
		
		sim.init(configuration);
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
	}
	
	/**
	 * Create a simulation with a controller.
	 * @param controller The controller controlling this simulation
	 */
	public Simulation (SimulationController controller){
		simulationControl = controller;
	}
	
	/*
	 **************************************
	 * public methods
	 **************************************
	 */

	public void init(String[] args){
		if (running) return;
		Logger.log("initialising data ");
		planning = true;
		getInterpreter().interpret(args);
		
		setup();

		simThread = new RunSim();
		Thread thread = new Thread(simThread);
		thread.start();
		Logger.say("scenario name " + this.myScenario.getName());

		deployMode();
	}
	
	public void startSimulation(){
		if (!planning ) return;
		battleMode();
	}

	public void endSimulation(){
		Logger.log("simulation ended at " + gameClock.toString());
		Logger.log("current time " + Instant.now().toString());
		//TODO
	}
	
	/*
	 **************************************
	 * private methods
	 **************************************
	 */
	
	private void setup(){
		// TODO do setup stuff prior to initial planning like relocate
		//TODO need to validate entities - on map etc.
		relocateEntities();
		//relocatePrepos(); // see relocate.f
	}
	
	private void relocateEntities(){
		//TODO make sure all entities are on the map.
		for (String entityName : getScenario().getEntityList().keySet()){
			Entity entity = getScenario().getEntityList().getEntity(entityName);
			//Logger.log(this, "entity " + entityName); //TODO remove
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
 * TODO
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
		Logger.log("deploy mode " + getScenario().getName());
		Logger.log("current time " + Instant.now().toString());

		//TODO check for start command
		if ( simulationControl == null){
			Logger.log("no simulation control defined ");
			battleMode();
		}
	}

	private void battleMode(){
		if (battleMode) return;
		planning = false;
		Logger.log("battle mode " + getScenario().getName());
		initQueue();
		
		Logger.log("current time " + Instant.now().toString());
		Logger.log("starting simulation " + getScenario().getName());
		Logger.log("start time " + Parser.formatTime(myScenario.getParameters().getStartTime()));
		Logger.log("end time   " + Parser.formatTime(myScenario.getParameters().getEndTime()));
		gameClock.setClockSecs(myScenario.getParameters().getStartTime());
		battleMode = true;

	}
	
	private void initQueue(){ //TODO add all the extra event types
		Logger.log("initialising queues");
		eventQueue.add(new ClockUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this));
		/* TODO
		eventQueue.add(new CloudUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this)); // TODO need different start time
		eventQueue.add(new ChemicalUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this)); // TODO need different start time
		eventQueue.add(new StatusUpdateEvent(
				this.getScenario().getParameters().getStartTime(), 
				this)); // TODO need different start time
		*/
		for (String entityName : getScenario().getEntityList().keySet()){
			Entity entity = getScenario().getEntityList().getEntity(entityName);
			//Logger.log(this, "entity " + entityName); //TODO remove
			eventQueue.add(EventFactory.makeMoveEvent(entity, this));
			eventQueue.add(EventFactory.makeDetectEvent(entity, this));
			eventQueue.add(EventFactory.makeScanEvent(entity, this));
			eventQueue.add(EventFactory.makeShootEvent(entity, this));
			
			// supply, suppression, ,  
			// cas assess, 
			// detect  obs, 
			// firing?, impact, active sensors, doarty
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
			running = false;
		}
	}

	/*
	 * simple accessors and mutators
	 */

	public EventQueue getEventQueue(){return eventQueue;}
	public CommandInterpreter getInterpreter() {return myInterpreter;}
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

	
	class RunSim implements Runnable{
		@Override
		public void run() {
			if (myScenario == null) return;
			if (eventQueue == null) return;
			running = true;
			while ( running){
				doUserEvents();
				if (battleMode){
					doSimEvents();
				}
			}
			endSimulation();
		}
	}
	
}
