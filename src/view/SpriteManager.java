package view;

import java.awt.Color;
import java.awt.Graphics;

import data.managers.EntityList;
import data.map.Coordinate;
import data.map.MapTransposer;
import sim.entity.Entity;
import sim.entity.EntityListener;
import utils.Logger;

public class SpriteManager implements EntityListener{
	
	private EntityList entityList;
	
	public void setEntityList (EntityList list){
		entityList = list;
	}

	@Override
	public void updateLocation(Entity entity, Coordinate location) {
		// TODO Auto-generated method stub
		
	}

	public void drawEntities(Graphics g, MapTransposer mapTransposer){
		int size = 20;
		if (entityList == null) return;
		for (Object object : entityList.values()) {
			Entity entity = (Entity) object;
			Coordinate c = entity.getLocation();
			double x = c.getX();
			double y = c.getY();
			int ix = mapTransposer.map2screenX(x);
			ix = ix - (size/ 2);
			int iy = mapTransposer.map2screenY(y);
			iy = iy + (size/2);
			g.setColor(Color.BLUE);
			g.fillOval(ix, iy, size, size);
		}
		return;
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
