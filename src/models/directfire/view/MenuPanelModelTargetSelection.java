package models.directfire.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Set;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import data.CSData;
import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.PlatformWeapon;
import data.csd.Weapon;
import data.csd.WeaponChoice;
import data.map.Coordinate;
import data.view.MenuPanel;
import sim.Constants;
import sim.Scenario;
import sim.entity.Entity;
import sim.forces.Force;
import utils.ReportPanel;
import utils.Tracer;
import view.TraceListener;

public class MenuPanelModelTargetSelection extends MenuPanel implements ActionListener, TraceListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	ReportPanel reportPanel = new ReportPanel();
	ReportPanel display = new ReportPanel(); 
	private double time = 0.0;
	private double tof = 0.0;
	private double uploadTime = 0.0;
//	private Entity e0; //TODO
	private CSData csdata;
	private Scenario scenario;
	
	private JComboBox<String> firerBox;

	public static void main(String args[]){
		MenuPanelModelTargetSelection me = new MenuPanelModelTargetSelection();
		me.test();
		}
	
	private void test(){
		setCSData(new CSData());
		setScenario( new Scenario("test_scenario"));
		makeData();
		JFrame frame = new JFrame("test MenuPanelTargetSelection");
		frame.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);;
					}
				}
		);
		JPanel c = getControlPanel();
		frame.add(c, BorderLayout.WEST);
		frame.add(reportPanel, BorderLayout.CENTER);
		frame.setSize(1000, 600);
		frame.setVisible(true);
		frame.validate();
		Tracer.addListener(this);
		update();
	}
	
	private JPanel getControlPanel(){
		JPanel controlPanel = new JPanel();
		controlPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE));
		
		JPanel timeLine = new JPanel();
		timeLine.add(new JLabel("Event Time: "));
		JTextField timeText = new JTextField("1.0",10);
		timeLine.add(timeText);

		JPanel firerLine = new JPanel();
		
		//TODO
		String[] firerList;
		Set<String> set = scenario.getEntityList().keySet();
		firerList = set.toArray(new String[set.size()]);
		firerBox = new JComboBox<String>(firerList);
		firerBox.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				update();
			}
		});

//		firerEntityName.setEnabled(false);
//		firerPlatformName.setEnabled(false);
		firerLine.add(new JLabel("Firer Name: "));
		firerLine.add(firerBox);
//		firerLine.add(new JLabel("Platform Name: "));
//		firerLine.add(firerPlatformName);
//		firerLine.add(firerX);
//		firerLine.add(firerY);
		
		// 6 weapons
		JPanel weaponLine1 = new JPanel(new BorderLayout());
		weaponLine1.add((new JLabel("weapons")),BorderLayout.NORTH);
//		weaponLine1.add(new JLabel("Weapon 1: "));
//		weaponLine1.add(new JLabel("a"));
		
//		Vector<EntityDetection> v = e0.getDetectionList();

		JPanel target1 = new JPanel();
		target1.add(new JLabel("Target1 " ));
		JPanel target2 = new JPanel();
		target2.add(new JLabel("Target2 " ));
		JPanel target3 = new JPanel();
		target3.add(new JLabel("Target3 " ));
		
		JPanel buttons = new JPanel();
		JButton b = new JButton("Execute");
		b.addActionListener(this);
		buttons.add(b);

//		controlPanel.setLayout(new GridLayout(7,1));
		controlPanel.setLayout(new BorderLayout());
		controlPanel.add(display, BorderLayout.CENTER);
//		controlPanel.add(timeLine);
		controlPanel.add(firerLine, BorderLayout.NORTH);
//		controlPanel.add(weaponLine1);
//		controlPanel.add(target1);
//		controlPanel.add(target2);
//		controlPanel.add(target3);
		controlPanel.add(buttons, BorderLayout.SOUTH);
		
		return controlPanel;
	}
	
	@Override
	public void actionPerformed(ActionEvent event) {
		String action = event.getActionCommand();
		System.out.println(action);
//		this.changePanel(action, "");
//		time = Double.parseDouble(timeText.getText());
		calculate();
	}
	
	//TODO
//	private String getFirerName(){return e0.getName();}
//	private String getFirerPlatformName(){return e0.getPlatform().getName();}
	
	private void makeData(){
		time = Math.random()*10 + 10.0;
		
		Force force = new Force("force");
		scenario.getForceList().add(force);
		Force enemyForce = new Force("enemy");
		scenario.getForceList().add(enemyForce);

		Platform p0 = new Platform("sniper");
		csdata.getPlatformList().add(p0);
		p0.setPrimaryRange(2.0);

		Platform p1 = new Platform("target_platform1");
		csdata.getPlatformList().add(p1);
		Platform p2 = new Platform("target_platform2");
		csdata.getPlatformList().add(p2);
		Platform p3 = new Platform("target_platform3");
		csdata.getPlatformList().add(p3);

		p0.setTargetPriority(p1.getName(), 1);
		p0.setTargetPriority(p2.getName(), 5);
		p0.setTargetPriority(p3.getName(), 10);

		Weapon ws1 = new Weapon("short1");
		csdata.getWeaponList().add(ws1);
		Weapon ws2 = new Weapon("short2");
		csdata.getWeaponList().add(ws2);
		Weapon ws3 = new Weapon("short3");
		csdata.getWeaponList().add(ws3);
		Weapon wl1 = new Weapon("long1");
		csdata.getWeaponList().add(wl1);
		Weapon wl2 = new Weapon("long2");
		csdata.getWeaponList().add(wl2);
		Weapon wl3 = new Weapon("long3");
		csdata.getWeaponList().add(wl3);
		
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
		
		csdata.getPHTableList().add(phs11);
		csdata.getPHTableList().add(phs12);
		csdata.getPHTableList().add(phs13);
		csdata.getPHTableList().add(phs21);
		csdata.getPHTableList().add(phs22);
		csdata.getPHTableList().add(phs23);
		csdata.getPHTableList().add(phs31);
		csdata.getPHTableList().add(phs32);
		csdata.getPHTableList().add(phs33);

		csdata.getPHTableList().add(phl11);
		csdata.getPHTableList().add(phl12);
		csdata.getPHTableList().add(phl13);
		csdata.getPHTableList().add(phl21);
		csdata.getPHTableList().add(phl22);
		csdata.getPHTableList().add(phl23);
		csdata.getPHTableList().add(phl31);
		csdata.getPHTableList().add(phl32);
		csdata.getPHTableList().add(phl33);

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

		csdata.getPKTableList().add(pks11);
		csdata.getPKTableList().add(pks12);
		csdata.getPKTableList().add(pks13);
		csdata.getPKTableList().add(pks21);
		csdata.getPKTableList().add(pks22);
		csdata.getPKTableList().add(pks23);
		csdata.getPKTableList().add(pks31);
		csdata.getPKTableList().add(pks32);
		csdata.getPKTableList().add(pks33);

		csdata.getPKTableList().add(pkl11);
		csdata.getPKTableList().add(pkl12);
		csdata.getPKTableList().add(pkl13);
		csdata.getPKTableList().add(pkl21);
		csdata.getPKTableList().add(pkl22);
		csdata.getPKTableList().add(pkl23);
		csdata.getPKTableList().add(pkl31);
		csdata.getPKTableList().add(pkl32);
		csdata.getPKTableList().add(pkl33);

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
		
		Entity e0 = new Entity("eagle_eye",p0);
		scenario.getEntityList().add(e0);
		e0.setLocation(new Coordinate(0, 0));
		e0.setForce(force);
		e0.setUserTargetPriority(0);
		e0.setUserPhThreshold(0.0);

		Entity e1 = new Entity("target_entity1",p1);
		Entity e2 = new Entity("target_entity2",p2);
		Entity e3 = new Entity("target_entity3",p3);
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
		
//		e0.setCommandWeapon(ews1);
//		e1.setManualWeapon(ew1);
		
//		e1.setCarrier(null); // TODO Fudge
		
		//e0.setUploadTime(uploadTime);
		//e0.setCommandTarget(e1);
		//e0.setCommandRounds(3);

		/*
		e1.setLocation(100.0 - 0.5 + Math.random(), 100.0 - 0.5 + Math.random());
		e2.setLocation(100.0 - 0.5 + Math.random(), 100.0 - 0.5 + Math.random());
		e2.setDefilade(Constants.DEFILADE_FULL);
		double range = Utils.range(e1, e2);
		*/
	}
	
	private void calculate(){
		String name = (String) firerBox.getSelectedItem();
		Entity e = scenario.getEntityList().getEntity(name);
		e.setUploadTime(uploadTime);
		Scenario scenario = new Scenario(); // TODO set the time
		//FIXME TargetSelectionEvent event = new TargetSelectionEvent(time, scenario, e);
		//FIXME event.doEvent();
		e.setLastTarget(e.getMyTarget());
		e.setLastWeapon(e.getMyWeapon());
	}

	public void writeTrace(String message){
		reportPanel.append(message);
	}
	
	private void update(){
		String name = (String) firerBox.getSelectedItem();
		Entity e = scenario.getEntityList().getEntity(name);
		display.clear();
		display.append("Event time: " + time);
		display.append("TOF: " + tof);
		display.append("Upload time: " + uploadTime);
		display.append("Firer = " + e.getName());
	}
	
	public void setCSData(CSData csd){csdata = csd;}
	public void setScenario(Scenario s){scenario = s;}
}
