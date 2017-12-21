package utils;

import data.map.Coordinate;
import sim.Constants;
import sim.entity.Entity;
import sim.entity.FirerEntity;
import sim.entity.TargetEntity;

public class Utils {
	
	public static double range(Entity e1, Entity e2){ //TODO remove - replaced by coordinate.distance
		return Math.sqrt(rangeSquare(e1, e2));
	}
	
	public static double rangeSquare(Entity e1, Entity e2){ //TODO remove - replaved by coord
		Coordinate c1 = e1.getLocation();
		Coordinate c2 = e2.getLocation();
		return Coordinate.distanceSquare2D(c1, c2);
	}
	
	public static int getPHPosture(double range, FirerEntity firer, TargetEntity target){
		int posture = 0;
		if (firer.getCurrentSpeed()>0.0) posture = 8;
		if (target.getCurrentSpeed()>0.0){
			posture = posture + 4;
		}
		if (target.getDefilade()== Constants.DEFILADE_EXPOSED){
			posture = posture + 2;
		}
		double angle = Math.PI; // set to rear shot by default
		if (range >0.0){
			double dx = target.getLocation().getX() - firer.getLocation().getX();
			double dy = target.getLocation().getY() - firer.getLocation().getY();
			if ((dx != 0.0) | (dy != 0.0)){
				angle = Math.atan2(dx, dy);
			}
			angle = angle - target.getDirectionFace();
			while (angle> (Math.PI*2)){angle = angle - (Math.PI * 2);}
			while (angle< 0){angle = angle + (Math.PI * 2);}
			posture = posture + 1; //TODO
			
		} else {
			posture = posture + 0;
		}
		return posture;
	}

}
