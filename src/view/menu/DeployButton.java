package view.menu;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;

import data.map.Coordinate;
import data.map.MapTransposer;
import sim.entity.Entity;
import utils.Logger;

public class DeployButton extends MenuButton {
	
	private Entity myEntity;

	public DeployButton() {
		super("Deploy");
	}

	public void clickMap(Coordinate coordinate){
		Logger.say("DeployButton: hit map " + coordinate.toString());
		Entity entity = ((MenuController)menuGroup).getEntity(coordinate);
		if (entity == null) {
			deployEntity(coordinate);
		} else {
			Logger.say("DeployButton: hit entity " + entity.getName());
			myEntity = entity;
		}
	}
	
	private void deployEntity(Coordinate coordinate){
		if (myEntity == null) return;
		// TODO clip to actual map
		myEntity.setLocation(coordinate);
		myEntity = null;
	}
	
	public void draw(Graphics g, MapTransposer mapTransposer){
		if (myEntity == null) return;
		highlightEntity(myEntity, g, mapTransposer);
	}
	
	public void backout(){
		myEntity = null;
	}
	
}
