package tests;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import data.CSData;
import interpreter.CommandInterpreter;
import models.EventQueue;
import models.movement.MoveEvent;
import sim.Scenario;
import sim.entity.Entity;
import utils.Tracer;
import view.ClockView;
import view.CommandView;
import view.FullFrame;
import view.TraceView;

public class TestEventGuiOld {

	private CSData myData = new CSData();
	private Scenario scenario = new Scenario(false);
	private CommandInterpreter interpreter = new CommandInterpreter(myData, scenario);
	private EventQueue queue = new EventQueue();

	public static void main(String[] args){
		TestEventGuiOld test = new TestEventGuiOld("test", args);
	}
	
	public TestEventGuiOld(String label, String[] args){
		makeFrame(label);
		interpreter.setTrace(true);
		interpreter.interpret(args);
	}
	
	private void makeFrame(String label){
		
		CommandView commandView = new CommandView();
		commandView.setInterpreter(interpreter);
		ClockView clockView = new ClockView();
		scenario.addClockListener(clockView);
		clockView.addActionListener(scenario.getClockController());
		FullFrame frame = new FullFrame(label);
		TraceView traceView = new TraceView();
		Tracer.addListener(traceView);
		frame.add(traceView,BorderLayout.CENTER);
		JButton startBtn = new JButton("Start");
		startBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				runTest();
			}
		});
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(startBtn, BorderLayout.WEST);
		panel.add(commandView,BorderLayout.CENTER);
		panel.add(clockView, BorderLayout.SOUTH);
		frame.add(panel,BorderLayout.NORTH);
//		frame.add(view.makeControlPanel(),BorderLayout.SOUTH);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
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

		scenario.setClock(scenario.getParameters().getStartTime());
		
		populateQueue();

		Tracer.write("start time " + scenario.getClock());
		Tracer.write("end time   " + scenario.getParameters().getEndTime());
		Runnable runnable = new RunSim(queue);
		Thread thread = new Thread(runnable);
		thread.start();
	}
	
	// this is the bit that should be overridden
	protected void populateQueue(){
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			/* FIXME
			Entity entity = scenario.getEntityList().getEntity(i);
			MoveEvent event = new MoveEvent(scenario.getParameters().getStartTime(), scenario, entity);
			event.setTime(scenario.getParameters().getStartTime() + 
					scenario.getParameters().getMovementCycleTime() * Math.random());
			queue.add(event);
			*/
		}
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
