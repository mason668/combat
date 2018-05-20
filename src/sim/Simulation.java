package sim;

import java.time.Instant;

import data.CSData;
import interpreter.CommandInterpreter;
import models.DetectEvent;
import models.EventFactory;
import models.EventQueue;
import models.MoveEvent;
import models.ScanEvent;
import models.ShootEvent;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;
import view.ClockListener;

public class Simulation {
	
	/*
	 * Class attributes
	 */
	
	private Scenario myScenario = new Scenario("simulation_test");
	private GameClock gameClock = new GameClock(myScenario);
	private CSData myData = new CSData();
	private CommandInterpreter myInterpreter = new CommandInterpreter(this);
	private boolean planning = false;
	private boolean battle = false;
	private boolean running = false;
	private EventQueue eventQueue = new EventQueue();
	private SimulationController simulationControl = null;
	private Runnable simThread;

	/**
	 * This class is executable. Running it runs a test.
	 * @param configuration arguments that may be used to configure the test. 
	 */
	public static void main(String[] configuration){
		Logger.say("running Simulation.main");
		Simulation sim = new Simulation();
		sim.getScenario().getParameters().setRealTimeSynch(false);
		//sim.getInterpreter().setTrace(true); //TODO not required
		// if there are no arguments, set some defaults to make the test run quickly
		
		// add a clock listener to report the time for the test
		/*
		sim.gameClock.addClockListener(new ClockListener(){
			public void updateClock(double clock) {
				Logger.log("time " + Parser.formatTime(clock));
			}
		});
		*/
		
		sim.init(configuration);
	}
	
	/*
	 * constructors
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
	 * public methods
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
		Logger.log("ending simulation ");
		//TODO
	}
	
	/*
	 * private methods
	 */
	
	private void setup(){
		// TODO do setup stuff prior to initial planning like relocate
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
		if (battle) return;
		planning = false;
		Logger.log("batle mode " + getScenario().getName());
		initQueue();
		
		Logger.log("current time " + Instant.now().toString());
		Logger.log("starting simulation " + getScenario().getName());
		Logger.log("start time " + Parser.formatTime(myScenario.getParameters().getStartTime()));
		Logger.log("end time   " + Parser.formatTime(myScenario.getParameters().getEndTime()));
		gameClock.setClock(myScenario.getParameters().getStartTime());
		battle = true;

	}
	
	class RunSim implements Runnable{
		@Override
		public void run() {
			if (myScenario == null) return;
			if (eventQueue == null) return;
			running = true;
			while ( running){
				doUserEvents();
				if (battle){
					doSimEvents();
				}
			}
			Logger.log("simulation ended at " + gameClock.toString());
			Logger.log("current time " + Instant.now().toString());
			endSimulation();
		}
		
		private void doUserEvents(){
			
		}
		
		private void doSimEvents(){
			// do sim events
			gameClock.incrementClock(myScenario.getParameters().getIncrementAmount());
			eventQueue.doEvents(gameClock.getClock());
			if (gameClock.getClock() >= myScenario.getParameters().getEndTime()){
				running = false;
			}
		}
	}
	
	private void initQueue(){ //TODO add all the extra event types
		Logger.log("initialising queues");
		for (String entityName : getScenario().getEntityList().keySet()){
			Entity entity = getScenario().getEntityList().getEntity(entityName);
			//Logger.log(this, "entity " + entityName);
			
			MoveEvent m = EventFactory.makeMoveEvent(entity, this);
			eventQueue.add(m);
			DetectEvent d = EventFactory.makeDetectEvent(entity, this);
			eventQueue.add(d);
			ScanEvent s = EventFactory.makeScanEvent(entity, this);
			eventQueue.add(s);
			ShootEvent f = EventFactory.makeShootEvent(entity, this);
			eventQueue.add(f);
			
			//entity.setTracing(true);
			
			// supply, suppression, ,  detect enemy/ friend/ obs, 
			// cas assess, 
			// clouds, statwt, chem, firing?, impact, active sensors, doarty
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
	
}
