package models.directfire;

import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.PlatformWeapon;
import data.csd.Weapon;
import data.csd.WeaponChoice;
import data.map.Coordinate;
import sim.Constants;
import sim.Scenario;
import sim.entity.Entity;
import sim.forces.Force;

public class TestTargetSelectionEvent {
	
	public static void main(String[] args){
		Scenario scenario = new Scenario();
		scenario.setClock(1.0);
		double eventTime = 1.0;
		
		Force force = new Force("force");
		scenario.getForceList().add(force);
		Force enemyForce = new Force("enemy");
		scenario.getForceList().add(enemyForce);

		Platform p0 = new Platform();
		
		p0.setPrimaryRange(2.0);
		
		Platform p1 = new Platform("target_platform1");
		Platform p2 = new Platform("target_platform2");
		Platform p3 = new Platform("target_platform3");
		p0.setTargetPriority(p1.getName(), 1);
		p0.setTargetPriority(p2.getName(), 5);
		p0.setTargetPriority(p3.getName(), 10);

		Weapon ws1 = new Weapon("short1");
		Weapon ws2 = new Weapon("short2");
		Weapon ws3 = new Weapon("short3");
		Weapon wl1 = new Weapon("long1");
		Weapon wl2 = new Weapon("long2");
		Weapon wl3 = new Weapon("long3");

		PHTable phs11 = new PHTable("phs1vs1");
		PHTable phs12 = new PHTable("phs1vs2");
		PHTable phs13 = new PHTable("phs1vs3");
		PHTable phs21 = new PHTable("phs2vs1");
		PHTable phs22 = new PHTable("phs2vs2");
		PHTable phs23 = new PHTable("phs2vs3");
		PHTable phs31 = new PHTable("phs3vs1");
		PHTable phs32 = new PHTable("phs3vs2");
		PHTable phs33 = new PHTable("phs3vs3");

		PHTable phl11 = new PHTable("phl1vs1");
		PHTable phl12 = new PHTable("phl1vs2");
		PHTable phl13 = new PHTable("phl1vs3");
		PHTable phl21 = new PHTable("phl2vs1");
		PHTable phl22 = new PHTable("phl2vs2");
		PHTable phl23 = new PHTable("phl2vs3");
		PHTable phl31 = new PHTable("phl3vs1");
		PHTable phl32 = new PHTable("phl3vs2");
		PHTable phl33 = new PHTable("phl3vs3");

		PKTable pks11 = new PKTable("pks1vs1");
		PKTable pks12 = new PKTable("pks1vs2");
		PKTable pks13 = new PKTable("pks1vs3");
		PKTable pks21 = new PKTable("pks2vs1");
		PKTable pks22 = new PKTable("pks2vs2");
		PKTable pks23 = new PKTable("pks2vs3");
		PKTable pks31 = new PKTable("pks3vs1");
		PKTable pks32 = new PKTable("pks3vs2");
		PKTable pks33 = new PKTable("pks3vs3");

		PKTable pkl11 = new PKTable("pkl1vs1");
		PKTable pkl12 = new PKTable("pkl1vs2");
		PKTable pkl13 = new PKTable("pkl1vs3");
		PKTable pkl21 = new PKTable("pkl2vs1");
		PKTable pkl22 = new PKTable("pkl2vs2");
		PKTable pkl23 = new PKTable("pkl2vs3");
		PKTable pkl31 = new PKTable("pkl3vs1");
		PKTable pkl32 = new PKTable("pkl3vs2");
		PKTable pkl33 = new PKTable("pkl3vs3");

		ws1.setPlatformPH(p1.getName(), phs11);
		ws1.setPlatformPH(p2.getName(), phs12);
		ws1.setPlatformPH(p3.getName(), phs13);
		ws2.setPlatformPH(p1.getName(), phs21);
		ws2.setPlatformPH(p2.getName(), phs22);
		ws2.setPlatformPH(p3.getName(), phs23);
		ws3.setPlatformPH(p1.getName(), phs31);
		ws3.setPlatformPH(p2.getName(), phs32);
		ws3.setPlatformPH(p3.getName(), phs33);

		wl1.setPlatformPH(p1.getName(), phl11);
		wl1.setPlatformPH(p2.getName(), phl12);
		wl1.setPlatformPH(p3.getName(), phl13);
		wl2.setPlatformPH(p1.getName(), phl21);
		wl2.setPlatformPH(p2.getName(), phl22);
		wl2.setPlatformPH(p3.getName(), phl23);
		wl3.setPlatformPH(p1.getName(), phl31);
		wl3.setPlatformPH(p2.getName(), phl32);
		wl3.setPlatformPH(p3.getName(), phl33);

		ws1.setPlatformPK(p1.getName(), pks11);
		ws1.setPlatformPK(p2.getName(), pks12);
		ws1.setPlatformPK(p3.getName(), pks13);
		ws2.setPlatformPK(p1.getName(), pks21);
		ws2.setPlatformPK(p2.getName(), pks22);
		ws2.setPlatformPK(p3.getName(), pks23);
		ws3.setPlatformPK(p1.getName(), pks31);
		ws3.setPlatformPK(p2.getName(), pks32);
		ws3.setPlatformPK(p3.getName(), pks33);

		wl1.setPlatformPK(p1.getName(), pkl11);
		wl1.setPlatformPK(p2.getName(), pkl12);
		wl1.setPlatformPK(p3.getName(), pkl13);
		wl2.setPlatformPK(p1.getName(), pkl21);
		wl2.setPlatformPK(p2.getName(), pkl22);
		wl2.setPlatformPK(p3.getName(), pkl23);
		wl3.setPlatformPK(p1.getName(), pkl31);
		wl3.setPlatformPK(p2.getName(), pkl32);
		wl3.setPlatformPK(p3.getName(), pkl33);

		PlatformWeapon slots1 = new PlatformWeapon(ws1);
		PlatformWeapon slots2 = new PlatformWeapon(ws2);
		PlatformWeapon slots3 = new PlatformWeapon(ws3);

		PlatformWeapon slotl1 = new PlatformWeapon(wl1);
		PlatformWeapon slotl2 = new PlatformWeapon(wl2);
		PlatformWeapon slotl3 = new PlatformWeapon(wl3);

		p0.addWeapon(slots1);
		p0.addWeapon(slots2);
		p0.addWeapon(slots3);
		p0.addWeapon(slotl1);
		p0.addWeapon(slotl2);
		p0.addWeapon(slotl3);

		p0.addWeaponChoice(new WeaponChoice(p1, 0.5, slots1, slotl1));
		p0.addWeaponChoice(new WeaponChoice(p2, 0.5, slots2, slotl2));
		p0.addWeaponChoice(new WeaponChoice(p3, 0.5, slots3, slotl3));

		Entity e0 = new Entity("shooter", p0);
		scenario.getEntityList().add(e0);
		e0.setLocation(new Coordinate(0.0, 0.0));
		e0.setForce(force);
		e0.setUserTargetPriority(0);
		e0.setUserPhThreshold(0.0);

		Entity e1 = new Entity("target_1",p1);
		Entity e2 = new Entity("target_2",p2);
		Entity e3 = new Entity("target_3",p3);
		scenario.getEntityList().add(e1);
		scenario.getEntityList().add(e2);
		scenario.getEntityList().add(e3);
		
		e1.setForce(enemyForce);
		e2.setForce(enemyForce);
		e3.setForce(enemyForce);

		e1.setNumberOfElements(1);
		e2.setNumberOfElements(1);
		e3.setNumberOfElements(3);

		e1.setLocation(new Coordinate(1.0, 1.0));
		e2.setLocation(new Coordinate(1.0, 0.8));
		e3.setLocation(new Coordinate(0.8, 1.0));

//		e1.setCarrier(null);
//		e2.setCarrier(null);
//		e3.setCarrier(null);

		e0.updateDetection(e1, Constants.OBSERVATION_LEVEL_IDENTIFIED);
		e0.updateDetection(e2, Constants.OBSERVATION_LEVEL_IDENTIFIED);
		e0.updateDetection(e3, Constants.OBSERVATION_LEVEL_IDENTIFIED);
		
		TargetSelectionEvent event = new TargetSelectionEvent(eventTime, scenario, e0);
		event.doEvent();
	}
	

}
