package tests;

import data.CSData;
import interpreter.CommandInterpreter;
import models.Event;
import models.EventQueue;
import models.movement.MoveEvent;
import sim.Scenario;
import sim.entity.Entity;
import utils.Tracer;

public class TestEvent {

	// these bits need to change depending on the test
	// Should be overridden as required
	protected Scenario scenario = new Scenario(false);
	protected static final String name = "TestMoveEvent";
	protected double epoch = scenario.getParameters().getMovementCycleTime();
	protected Event makeEvent(Entity entity){
		return new MoveEvent(scenario.getParameters().getStartTime(), scenario, entity);		
	}
	
	protected CSData myData = new CSData();
	protected CommandInterpreter interpreter = new CommandInterpreter(myData, scenario); //TODO maybe private
	protected EventQueue queue = new EventQueue();

	public static void main(String[] args){
		TestEvent test = new TestEvent(args);
		test.test(name);
	}
	
	public TestEvent(String[] args){
		interpreter.setTrace(true);
		interpreter.interpret(args);
	}
	
	public CommandInterpreter getInterpreter(){return interpreter;}
	public Scenario getScenario(){return scenario;}

	protected void populateQueue(){
		for (String entityName : scenario.getEntityList().keySet()){
			Entity entity = scenario.getEntityList().getEntity(entityName);
			Event event = makeEvent(entity);
			event.setTime(scenario.getParameters().getStartTime() + 
					epoch * Math.random());
			queue.add(event);
		}
	}
	
	protected void test(String testName){
		Tracer.setEcho(true);
		Tracer.write("Running test");
		
//		Tracer.setLevel(this.trace);
		Tracer.write(Tracer.MOVEMENT, 0, "Testing class " + testName);
		
//		listData(); //TODO should use actual values from scenario etc

		//TODO display error message etc
		if (scenario.getEntityList().getSize()<= 0){
			Tracer.write("no entities");
			return;
		}

		scenario.setClock(scenario.getParameters().getStartTime());
		populateQueue();
		Tracer.write("start time " + scenario.getClock());
		Tracer.write("end time   " + scenario.getParameters().getEndTime());
		scenario.getParameters().setRealTimeSynch(false);
		runTest();
	}
	
	protected void runTest(){
		Tracer.write("running test now");
		while (scenario.getClock() < scenario.getParameters().getEndTime()){
			queue.doEvents(scenario.getClock());
			scenario.incrementClock(1.0);
			updateData();
		}
	}
	
	protected void updateData(){}
	
}
