package sim;

import data.CSData;
import interpreter.CommandInterpreter;
import models.Event;
import models.EventQueue;
import models.movement.MoveEvent;
import sim.entity.Entity;
import utils.Logger;
import utils.Parser;
import view.ClockListener;

public class Simulation implements ClockListener{
	
	private Scenario myScenario = new Scenario("simulation_test");
	private CSData myData = new CSData();
	private CommandInterpreter myInterpreter = new CommandInterpreter(myData, myScenario);

	public Scenario getScenario(){return myScenario;}
	public CommandInterpreter getInterpreter() {return myInterpreter;}
	
	public static void main(String[] args){
		Simulation sim = new Simulation();
		sim.myScenario.getParameters().setRealTimeSynch(false);
		//sim.myInterpreter.setTrace(true);
		sim.myInterpreter.interpret(args);
		//sim.myScenario.addClockListener(sim); // add this as a clock listener to see the time
		if (!sim.myScenario.isRunning()){sim.myScenario.start();}
	}
	@Override
	public void updateClock(double clock) {
		Logger.say("clock: " + Parser.formatTime(clock));
	}

}
