package models.movement;

import data.CSData;
import interpreter.CommandInterpreter;
import models.EventQueue;
import models.movement.MoveEvent;
import sim.Scenario;
import sim.entity.Entity;
import utils.Tracer;

public class TestMoveEvent {

	private CSData myData = new CSData();
	private Scenario scenario = new Scenario(false);
	private CommandInterpreter interpreter = new CommandInterpreter(myData, scenario);

	public static void main(String[] args){
		TestMoveEvent test = new TestMoveEvent();
		test.test(args);
	}
	
	private void test(String[] args){
//		makeFrame();
		interpreter.setTrace(true);
		interpreter.interpret(args);
		runTest();
	}
	
	private void runTest(){
		Tracer.setEcho(true);
		Tracer.write("Running test");
		
//		Tracer.setLevel(this.trace);
		Tracer.write(Tracer.MOVEMENT, 0, "Testing class MoveEvent");
		
//		listData(); //TODO should use actual values from scenario etc

		//TODO display error message etc
		if (scenario.getEntityList().getSize()<= 0){
			Tracer.write("no entities");
			return;
		}

		EventQueue queue = new EventQueue();
		scenario.setClock(scenario.getParameters().getStartTime());
		//TODO
		/*
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			Entity entity = scenario.getEntityList().getEntity(i);
			MoveEvent event = new MoveEvent(scenario.getParameters().getStartTime(), scenario, entity);
			event.setTime(scenario.getParameters().getStartTime() + 
					scenario.getParameters().getMovementCycleTime() * Math.random());
			queue.add(event);
		}
		*/
		Tracer.write("start time " + scenario.getClock());
		Tracer.write("end time   " + scenario.getParameters().getEndTime());
		scenario.getParameters().setRealTimeSynch(false);
		while (scenario.getClock() < scenario.getParameters().getEndTime()){
			queue.doEvents(scenario.getClock());
			scenario.incrementClock(1.0);
		}
	}
	
}
