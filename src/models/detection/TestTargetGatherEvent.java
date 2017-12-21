package models.detection;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import data.CSData;
import data.csd.Flyer;
import data.csd.Platform;
import data.csd.Sensor;
import data.map.Coordinate;
import interpreter.CommandInterpreter;
import models.Event;
import models.EventQueue;
import sim.Constants;
import sim.Scenario;
import sim.entity.DetectedEntity;
import sim.entity.Entity;
import sim.entity.EntityDetection;
import sim.forces.Force;
import tests.TestView;
import utils.Parser;
import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class TestTargetGatherEvent {

	private CSData myData = new CSData();
	private Scenario scenario = new Scenario(false); // needs to be non-runnable to avoid "start" command in interpreter
	private CommandInterpreter interpreter = new CommandInterpreter(myData, scenario);

	public static void main(String[] args){
		TestTargetGatherEvent test = new TestTargetGatherEvent();
//		test.doArgs(args); //TODO not all arguments available to testTargetGather
		test.test(args);
	}
	
	//TODO this all needs replacing
	private int trace = Tracer.FINEST;
	private double eventTime = 0.0;
	private double testClock = 0.0;
	private int testLoop = 3;
	private Sensor testSensor = new Sensor("");
	private double testMapMin = 0.0;
	private double testMapMax = 10.0;
	private int testFriends = 5;
	private int testEnemies = 5;
	private int testNeutral = 5;
	private int testTargetListSize = 20;

	private Platform platform1 = new Platform("platform1");
	private Platform platform2 = new Platform("platform2");
	private Platform platform3 = new Platform("platform3");
	
	private int testPriority1 = 1;
	private int testPriority2 = 2;
	private int testPriority3 = 3;
	
	private void logArguments(){
		Tracer.write("values used in test:");
		Tracer.write("testLoop " + testLoop);
		Tracer.write("testSensor " + testSensor.getName());
		Tracer.write("testFriends " + testFriends);
		Tracer.write("testEnemies " + testEnemies);
		Tracer.write("testNeutral " + testNeutral);
		Tracer.write("testTargetListSize " + testTargetListSize);
		Tracer.write("priority1 " + testPriority1);
		Tracer.write("priority2 " + testPriority2);
		Tracer.write("priority3 " + testPriority3);
		Tracer.write("");
	}

	private void testa(){
		//TODO need to include all the detection levels and relationships in the new test routine
		logArguments();
		Tracer.setLevel(this.trace);
//		SimpleView view = new SimpleView();
//		Tracer.addListener(view);
		
		this.platform3.setFlyerType(new Flyer());

		Scenario scenario = new Scenario();
		scenario.setClock(this.testClock);
		
		Force blueForce = new Force("blue");
		scenario.getForceList().add(blueForce);
		Force redForce = new Force("red");
		scenario.getForceList().add(redForce);
		Force greenForce = new Force("green");
		scenario.getForceList().add(greenForce);
		
		blueForce.setNumberOfTargets(testTargetListSize);

		Platform observerPlatform = new Platform("observer_platform");
		observerPlatform.setDetectionPriority(platform1, testPriority1);
		observerPlatform.setDetectionPriority(platform2, testPriority2);
		observerPlatform.setDetectionPriority(platform3, testPriority3);

		Entity observer = new Entity("observer",observerPlatform);
		observer.setForce(blueForce);
		observer.setLocation(new Coordinate(testMapMin + 
					Math.random()*(testMapMax - testMapMin),
				testMapMin + 
					Math.random()*(testMapMax - testMapMin)));
		scenario.getEntityList().add(observer);
//		observer.setCurrentSensor(this.testSensor);

		for (int i = 0;i<testFriends;i++) {
			makeTargets("friend", i, blueForce, scenario, observer);
		}
		for (int i = 0;i<testEnemies;i++) {
			makeTargets("enemy", i, redForce, scenario, observer);
		}
		for (int i = 0;i<testNeutral;i++) {
			makeTargets("neutral", i, greenForce, scenario, observer);
		}
		listEntities(scenario);

		TargetGatherEvent event = new TargetGatherEvent(this.eventTime, scenario, observer);
		event.setTime(this.eventTime);
		double clock = this.eventTime;
		for (int i=0;i<testLoop;i++){
			Event newEvent = event.doEvent(); // TODO 
			listTargets(observer);
			clock = clock + blueForce.getScanTime();
			scenario.setClock(clock);
			updateLocations(scenario);
			updateDetections(observer);
		}
		
//		while (true){}
	}
	
	private void makeTargets(String name, int i, Force force, Scenario scenario, Entity observer){
		double d = Math.random();
		Platform p;
		if (d > 0.66){
			p = platform1;
		} else if (d > 0.33){
			p = platform2;
		} else {
			p = platform3;
		}
		Entity e = new Entity(name + Parser.pad(""+(i+1), 3, "0"),p);
		e.setForce(force);
		e.setLocation(new Coordinate(testMapMin + 
				Math.random()*(testMapMax - testMapMin),
			testMapMin + 
				Math.random()*(testMapMax - testMapMin)));
		scenario.getEntityList().add(e);
		int level = Constants.OBSERVATION_LEVEL_UNSEEN;
		d = Math.random();
		if ( d > 0.75) {
			level = Constants.OBSERVATION_LEVEL_IDENTIFIED;
		} else if (d > 0.50) {
			level = Constants.OBSERVATION_LEVEL_RECOGNISED;
		} else if (d > 0.25) {
			level = Constants.OBSERVATION_LEVEL_DETECTED;
		}
		if ( level >= Constants.OBSERVATION_LEVEL_DETECTED) {
			observer.getDetectionList().add(
					new EntityDetection(e, level));
		}
	}
	
	private void updateDetections(Entity observer){
		for (int i = 0;i < observer.getTargetList().getSize();i++){
			DetectedEntity e = observer.getTargetList().get(i);
			Tracer.write("updating " + e.getName());
			int level = observer.getDetectionLevel(e);
			if (level > Constants.OBSERVATION_LEVEL_UNSEEN){
				level = Constants.OBSERVATION_LEVEL_UNSEEN;
				double d = Math.random();
				if ( d > 0.75) {
					level = Constants.OBSERVATION_LEVEL_IDENTIFIED;
				} else if (d > 0.50) {
					level = Constants.OBSERVATION_LEVEL_RECOGNISED;
				} else if (d > 0.25) {
					level = Constants.OBSERVATION_LEVEL_DETECTED;
				}
				observer.updateDetection((Entity)e, level);
			}
		}
	}
	
	private void updateLocations(Scenario scenario){
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			Entity e = scenario.getEntityList().getEntity("");
			e.setLocation(new Coordinate(testMapMin + 
					Math.random()*(testMapMax - testMapMin),
				testMapMin + 
					Math.random()*(testMapMax - testMapMin)));
		}
	}
	
	private void listEntities(Scenario scenario){
		Tracer.write("list of entities, force, platform");
		for (int i = 0; i<scenario.getEntityList().getSize();i++) {
			Entity e = scenario.getEntityList().getEntity("");
			Tracer.write(e.getName() + " " + e.getForce().getName() +
					" " + e.getPlatform().getName());
		}
	}
	
	private void listTargets(Entity observer){
		Tracer.write("list of targets");
		/*
		for (DetectedEntity e: observer.getTargetList()){
			Tracer.write(e.getName());
		}
		*/
		
	}

	private void test(String[] args){
		makeFrame();
		interpreter.setTrace(true);
		interpreter.interpret(args);
	}
	
	private void makeFrame(){
		TestView view = new TestView("Test target gather",
				interpreter, this.scenario, new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				runTest();
			}
		});
	}
	
	private void runTest(){
		Tracer.setEcho(true);
		Tracer.write("Running test");
//		Tracer.setLevel(this.trace);
		Tracer.write(Tracer.MOVEMENT, 0, "Testing class TargetGatherEvent"); // TODO shouldn't be movement
		//TODO display error message etc
		if (scenario.getEntityList().getSize()<= 0){
			Tracer.write("no entities");
			return;
		}
		
		EventQueue queue = new EventQueue();
		
//		listData(); //TODO should use actual values from scenario etc

		scenario.setClock(scenario.getParameters().getStartTime());
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			Entity entity = scenario.getEntityList().getEntity("");
			if (entity.getForce() == null) continue;
			TargetGatherEvent event = new TargetGatherEvent(scenario.getParameters().getStartTime(), 
					scenario, entity);
			event.setTime(scenario.getParameters().getStartTime() + 
					scenario.getParameters().getMovementCycleTime() * Math.random()); //FIXME
			queue.add(event);
		}
		Tracer.write("start time " + Parser.formatTime(scenario.getClock()));
		Tracer.write("end time   " + Parser.formatTime(scenario.getParameters().getEndTime()));
		Runnable runnable = new RunSim(queue);
		Thread thread = new Thread(runnable);
		thread.start();
	}

	class RunSim implements Runnable{
		
		EventQueue queue;
		
		public RunSim(EventQueue q){
			queue = q;
		}

		@Override
		public void run() {
			while (scenario.getClock() < scenario.getParameters().getEndTime()){
				queue.doEvents(scenario.getClock());
				scenario.incrementClock(1.0);
			}
		}
		
	}
}
