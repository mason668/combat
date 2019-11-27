package view.menu;

import data.map.Coordinate;
import utils.Logger;

public class TemplateButton extends MenuButton {

	public TemplateButton() {
		super("Deploy");
	}

	public void clickMap(Coordinate coordinate){
		//Logger.say("DeployButton: hit map " + coordinate.toString());
		/*
		Entity entity = menuController.getEntity(coordinate);
		if (entity == null) return; // TODO other reports
		menuController.report(getInfoReport(entity));
		*/
	}
	
}
