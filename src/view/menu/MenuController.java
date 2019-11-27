package view.menu;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import data.map.Coordinate;
import data.map.MapTransposer;
import interpreter.Interpreter;
import sim.Scenario;
import sim.Simulation;
import sim.entity.Entity;
import utils.Logger;
import view.map.MapListener;
import view.reports.ReportListener;

public class MenuController extends MenuGroup implements ActionListener, MapListener {
	
	@Override
	public void clickMap(Coordinate coordinate){
		//Logger.say("menuController: hit map " + coordinate.toString());
		if (currentMenuState == null) return;
		currentMenuState.clickMap(coordinate);
	}

	@Override
	public void moveMouse(Coordinate c) {
	}
	
	private Simulation mySim;
	public void setSimulation(Simulation sim){
		mySim = sim;
	}
	public Simulation getSimulation(){
		return mySim;
	}

	public Scenario getScenario(){
		if (mySim == null) return null;
		return mySim.getScenario();
	}
	public Interpreter getInterpreter(){
		if (mySim == null) return null;
		return mySim.getInterpreter();
	}
	
	public Entity getEntity(Coordinate coordinate){
		if (mySim == null) return null;
		return mySim.getScenario().getEntityList().getEntity(coordinate, 0.5); //TODO need range to depend on scale
	}
	
	private ReportListener reportListener;
	public void addReportLitener(ReportListener listener){
		reportListener = listener;
	}
	public void report(String message){
		//TODO could there be a list of listeners?
		if (reportListener == null) return;
		reportListener.write(message);
	}
	
	public void draw(Graphics g, MapTransposer mapTransposer){
		if (currentMenuState != null){
			currentMenuState.draw(g, mapTransposer);
		}
	}
}
