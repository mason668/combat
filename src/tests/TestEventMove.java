package tests;

import models.Event;
import models.movement.MoveEvent;
import sim.entity.Entity;

public class TestEventMove extends TestEvent {

	protected static final String name = "TestEventMove";
	protected double epoch = scenario.getParameters().getMovementCycleTime();
	protected Event makeEvent(Entity entity){
		return new MoveEvent(scenario.getParameters().getStartTime(), scenario, entity);		
	}

	public TestEventMove(String[] args) {
		super(args);
	}

	public static void main(String[] args){
		TestEventMove test = new TestEventMove(args);
		if (args.length<=0){
			test.getInterpreter().interpret("load test_move.txt");
		}
		test.test(name);
	}
}
