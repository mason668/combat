package models.directfire;

import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.Weapon;
import data.map.Coordinate;
import sim.Constants;
import sim.entity.Entity;
import data.csd.PlatformWeapon;
import utils.Logger;
import utils.Utils;

public class TestDirectFireImpact {
	public static void main(String args[]){
		Logger.setLevel(Logger.COURSE);
		double time = Math.random()*10 + 10.0;
		Weapon w = new Weapon("firer_weapon");
		PlatformWeapon s = new PlatformWeapon(w);
		Platform p1 = new Platform("firer_platform");
		p1.addWeapon(s);
		Platform p2 = new Platform("target_platform");
		PHTable ph = new PHTable("ph001");
		PKTable pk = new PKTable("pk001");
		w.setPlatformPH(p2.getName(), ph);
		w.setPlatformPK(p2.getName(), pk);
		Entity e1 = new Entity("firer_entity",p1);
		Entity e2 = new Entity("target_entity",p2);
		e1.setLocation(new Coordinate(100.0 - 0.5 + Math.random(), 100.0 - 0.5 + Math.random()));
		e2.setLocation(new Coordinate(100.0 - 0.5 + Math.random(), 100.0 - 0.5 + Math.random()));
		e2.setDefilade(Constants.DEFILADE_FULL);
		double range = Utils.range(e1, e2);
		
		DirectFireImpactEvent me = new DirectFireImpactEvent(time, e1, e2, w, 3, range);
		me.doEvent();
	}


}
