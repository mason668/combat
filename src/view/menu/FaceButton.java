package view.menu;

import java.awt.Color;
import java.awt.Graphics;

import data.map.Coordinate;
import data.map.MapTransposer;
import sim.entity.Entity;
import utils.Logger;

public class FaceButton extends MenuButton {

	private Entity myEntity;

	public FaceButton() {
		super("Face");
	}

	public void clickMap(Coordinate coordinate){
		Logger.say("FaceButton: hit map " + coordinate.toString());
		Entity entity = ((MenuController)menuGroup).getEntity(coordinate);
		if (entity == null) {
			Logger.say("FaceButton: hit map ");
			moveEntity(coordinate); //TODO wrong method name
		} else {
			Logger.say("FaceButton: hit entity " + entity.getName());
			myEntity = entity;
		}
		/*
		Entity entity = menuController.getEntity(coordinate);
		if (entity == null) return; // TODO other reports
		menuController.report(getInfoReport(entity));
		*/
	}
	
	//TODO rename
	private void moveEntity(Coordinate coordinate){
		if (myEntity == null) return;
		myEntity.setMoveTo(coordinate);
		// TODO clip to actual map
		//myEntity.setLocation(coordinate);
		//myEntity = null;
	}
	
	public void draw(Graphics g, MapTransposer mapTransposer){
		if (myEntity == null) return;
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
