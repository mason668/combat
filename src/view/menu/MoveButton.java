package view.menu;

import java.awt.Color;
import java.awt.Graphics;

import data.map.Coordinate;
import data.map.MapTransposer;
import interpreter.Interpreter;
import sim.entity.Entity;
import utils.Logger;

public class MoveButton extends MenuButton {

	private Entity myEntity;

	public MoveButton() {
		super("Move");
	}

	public void clickMap(Coordinate coordinate){
		Logger.say("MoveButton: hit map " + coordinate.toString());
		Entity entity = ((MenuController)menuGroup).getEntity(coordinate);
		if (entity == null) {
			Logger.say("MoveButton: hit map ");
			moveEntity(coordinate);
		} else {
			Logger.say("MoveButton: hit entity " + entity.getName());
			myEntity = entity;
		}
		/*
		Entity entity = menuController.getEntity(coordinate);
		if (entity == null) return; // TODO other reports
		menuController.report(getInfoReport(entity));
		*/
	}
	
	private void moveEntity(Coordinate coordinate){
		if (myEntity == null) return;
		//myEntity.setMoveTo(coordinate);
		Interpreter interpreter = ((MenuController)menuGroup).getInterpreter();
		if (interpreter == null){
			return;
		}
		interpreter.interpret(
				"entity " + myEntity.getName() + " move_to " +
				coordinate.getX() + " " + coordinate.getY()
				// TODO should use coord to utm
		);
		// TODO clip to actual map
	}
	
	public void draw(Graphics g, MapTransposer mapTransposer){
		if (myEntity == null) return;
		highlightEntity(myEntity, g, mapTransposer);
		
		Coordinate c = myEntity.getLocation();
		double x = c.getX();
		double y = c.getY();
		int ix1 = mapTransposer.map2screenX(x);
		int iy1 = mapTransposer.map2screenY(y);
		
		g.setColor(ENTITY_HIGHLIGHT);
		c = myEntity.getMoveTo();
		if (c == null) return;
		//Logger.say("MoveButton: hit map ");
		x = c.getX();
		y = c.getY();
		int ix2 = mapTransposer.map2screenX(x);
		int iy2 = mapTransposer.map2screenY(y);
		g.drawLine(ix1, iy1, ix2, iy2);
		
		g.drawLine(ix2, iy2-10, ix2+10, iy2);
		g.drawLine(ix2+10, iy2, ix2, iy2+10);
		g.drawLine(ix2, iy2+10, ix2-10, iy2);
		g.drawLine(ix2-10, iy2, ix2, iy2-10);
	}

	public void backout(){
		myEntity = null;
	}
	
}
