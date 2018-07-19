package view;

import java.awt.Color;
import java.awt.Graphics;

import data.managers.EntityList;
import data.map.Coordinate;
import data.map.MapTransposer;
import sim.entity.Entity;
import sim.entity.EntityListener;
import utils.Logger;
import view.menu.MenuController;

public class SpriteManager implements EntityListener{
	
	private EntityList entityList;
	private MenuController menuController;
	
	public void setEntityList (EntityList list){
		entityList = list;
	}
	
	public void setmenuController(MenuController controller){
		menuController = controller;
	}

	@Override
	public void updateLocation(Entity entity, Coordinate location) {
		// TODO Auto-generated method stub
	}

	public void drawEntities(Graphics g, MapTransposer mapTransposer){
		int size = 10;
		if (entityList == null) return;
		for (Object object : entityList.values()) {
			Entity entity = (Entity) object;
			Coordinate c = entity.getLocation();
			double x = c.getX();
			double y = c.getY();
			int ix = mapTransposer.map2screenX(x);
			int iy = mapTransposer.map2screenY(y);
			g.setColor(Color.BLUE);
			g.fillOval(ix-(size), iy-size, size*2, size*2);
			g.drawString(entity.getName(), ix-size, iy-size*2);
			g.setColor(Color.BLACK);
			g.fillOval(ix-1, iy-1, 3, 3);
		}
		drawGraphics(g, mapTransposer);
		return;
	}

	/**
	 * Draw the miscellaneous graphics such as view fans, based on menu selection
	 * @param g
	 * @param mapTransposer
	 */
	private void drawGraphics(Graphics g, MapTransposer mapTransposer){
		if (menuController == null) return;
		menuController.draw(g, mapTransposer);
	}

	
	/*
	private Polygon makePolygon(Iterator<Coordinate> coordinateIterator, AreaFeature area){
		int arrayX[] = new int[area.getCoordinateList().getSize()];
		int arrayY[] = new int[area.getCoordinateList().getSize()];
		int coordinateCounter = 0;
		while (coordinateIterator.hasNext()){
			Coordinate c = coordinateIterator.next();
			double x = c.getX();
			arrayX[coordinateCounter] = mapTransposer.map2screenX(x);
			double y = c.getY();
			arrayY[coordinateCounter] = mapTransposer.map2screenY(y);
			coordinateCounter++;
		}
		Polygon p = new Polygon(arrayX, arrayY, coordinateCounter);
		return p;
	}
	*/
}
