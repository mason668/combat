package view.menu;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

import javax.swing.JButton;
import javax.swing.JRadioButton;

import data.map.Coordinate;
import data.map.MapTransposer;
import sim.Scenario;
import sim.entity.Entity;
import utils.Logger;
import view.SmallButton;

public class MenuButton extends SmallButton{
	
	public final static Color COLOR_SELECTED = new Color(128, 255, 128);
	public final static Color COLOR_UNSELECTED = Color.WHITE;
	
	public MenuButton(String label){
		super(label);
		setFont(); // Use font from SmallButton
		//this.setEnabled(false);
		setBackground(COLOR_UNSELECTED);
	}
	
	public void select(){
		setBackground(COLOR_SELECTED);
	}
	public void unselect(){
		setBackground(COLOR_UNSELECTED);
	}
	
	protected MenuGroup menuGroup;

	public void setMenuGroup(MenuGroup group){
		menuGroup = group;
	}

	public void clickMap(Coordinate coordinate){
		Logger.say("MenuButton: hit map " + coordinate.toString());
	}
	/*
	private void setFont(){
		this.setFont(new Font("Arial", Font.PLAIN, 8));
	}
	*/
	
	public void backout(){
	}
	public void draw(Graphics g, MapTransposer mapTransposer){
	}

	public static final Color ENTITY_HIGHLIGHT = Color.GRAY;
	
	public void highlightEntity(Entity entity, Graphics g, MapTransposer mapTransposer){
		if (entity == null) return;
		Coordinate c = entity.getLocation();
		double x = c.getX();
		double y = c.getY();
		int ix1 = mapTransposer.map2screenX(x);
		int iy1 = mapTransposer.map2screenY(y);
		g.setColor(ENTITY_HIGHLIGHT);
		int size = 20;
		g.drawOval(ix1-(size), iy1-size, size*2, size*2);
	}

}
