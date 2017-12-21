package sim;

import data.CSData;
import interpreter.CommandInterpreter;
import models.movement.MoveEvent;
import sim.entity.Entity;
import utils.Tracer;

public class Test {

	private CSData myData = new CSData();
	private Scenario scenario = new Scenario();
	private CommandInterpreter interpreter = new CommandInterpreter(myData, scenario);

	public static void main(String[] args){
		Test test = new Test();
		test.test(args);
	}
	
	private void test(String[] args){
		Tracer.setEcho(true);
		Tracer.write("Running test");
		interpreter.interpret(args);
		
//		Tracer.setLevel(this.trace);
		Tracer.write(Tracer.MOVEMENT, 0, "Testing class MoveEvent");
		
//		listData(); //TODO should use actual values from scenario etc

		scenario.setClock(scenario.getParameters().getStartTime());
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			/* FIXME
			Entity entity = scenario.getEntityList().getEntity(i);
			MoveEvent event = new MoveEvent(scenario.getParameters().getStartTime(), scenario, entity);
			event.setTime(scenario.getParameters().getStartTime());
			event.doEvent();
			*/
		}

		/*
		if (test.testGUI){
			test.runGUI();
		} else {
			Tracer.setEcho(true);
			test.test();
		}
		*/
	}
}
