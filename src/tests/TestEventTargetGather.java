package tests;

import data.map.Coordinate;
import models.Event;
import models.detection.TargetGatherEvent;
import sim.Constants;
import sim.entity.DetectedEntity;
import sim.entity.Entity;

public class TestEventTargetGather extends TestEvent {

	protected static final String name = "TestEventTargetGather";
	protected double epoch = scenario.getParameters().getMovementCycleTime(); //TODO change
	// 			clock = clock + blueForce.getScanTime();

	protected Event makeEvent(Entity entity){
		return new TargetGatherEvent(scenario.getParameters().getStartTime(), scenario, entity);		
	}

	public TestEventTargetGather(String[] args) {
		super(args);
	}

	public static void main(String[] args){
		TestEventTargetGather test = new TestEventTargetGather(args);
		if (args.length<=0){
			test.getInterpreter().interpret("load test_detect.txt");
		}
		test.test(name);
	}
	// override
	//This should update actual detections and then randomly move targets
	// to see if they shift in the list
	protected void updateData(){
		updateDetections();
		updateLocations();
	}
	
	private void updateDetections(){
		for (String name: scenario.getEntityList().keySet()){
			Entity observer = scenario.getEntityList().getEntity(name);
			for (int i = 0;i < observer.getTargetList().getSize();i++){
				DetectedEntity e = observer.getTargetList().get(i);
				int level = Constants.OBSERVATION_LEVEL_UNSEEN;
				if (i == 0) level = Constants.OBSERVATION_LEVEL_IDENTIFIED;
				if (i == 1) level = Constants.OBSERVATION_LEVEL_RECOGNISED;
				if (i > 1){
					if (Math.random() > 0.25){
						level = Constants.OBSERVATION_LEVEL_DETECTED;
					}
				}
				observer.updateDetection((Entity)e, level);
			}
		}
	}
	
	private void updateLocations(){
		for (String name: scenario.getEntityList().keySet()){
			Entity e = scenario.getEntityList().getEntity(name);
			e.setLocation(new Coordinate(scenario.getMap().getLL().getX() + 
					Math.random()*(scenario.getMap().getWidth()),
					scenario.getMap().getLL().getY() + 
					Math.random()*(scenario.getMap().getHeight())));
		}
	}
}
