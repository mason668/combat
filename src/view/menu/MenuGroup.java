package view.menu;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import data.map.Coordinate;
import data.map.MapTransposer;
import sim.Scenario;
import sim.entity.Entity;
import utils.Logger;
import view.map.MapListener;
import view.reports.ReportListener;

public class MenuGroup implements ActionListener {
	
	protected MenuButton currentMenuState;
	protected Vector<MenuButton> buttonList = new Vector<MenuButton>();

	@Override
	public void actionPerformed(ActionEvent arg0) {
		Logger.say("menuController: hit button " + arg0.getActionCommand());
		MenuButton button = (MenuButton) arg0.getSource(); // TODO should validate before cast?
		//TODO is there a way to know which mouse button was used?
		setMenuState (button);
	}
	
	public void addMenuButton(MenuButton button){
		if (buttonList.contains(button)) return;
		buttonList.add(button);
		button.addActionListener(this);
		button.setMenuGroup(this);
	}
	
	public void setMenuState(MenuButton button){
		Logger.say("menuController: set state " + button.getActionCommand());
		if (!buttonList.contains(button)) return;
		if (button == currentMenuState) return;
		if (currentMenuState != null){
			currentMenuState.backout();
			currentMenuState.unselect();
		}
		currentMenuState = button;
		//button.setForeground(Color.RED);
		button.select();
	}
	
}
