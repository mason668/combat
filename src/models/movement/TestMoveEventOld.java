package models.movement;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import data.CSData;
import data.csd.CounterBatteryRadar;
import data.csd.Flyer;
import data.csd.Platform;
import data.csd.PlatformWeapon;
import data.csd.Weapon;
import data.map.Coordinate;
import interpreter.CommandInterpreter;
import sim.Constants;
import sim.obstacles.Pit;
import sim.Scenario;
import sim.entity.Entity;
import sim.entity.EntityWeapon;
import sim.forces.Force;
import sim.route.NodeGo;
import sim.route.NodeHold;
import sim.route.NodeLOS;
import sim.route.NodeStop;
import sim.route.Route;
import utils.Logger;
import utils.Tracer;
import view.ScenarioViewOld;
import view.TraceView;

public class TestMoveEventOld {

	private CSData myData;
	private Scenario scenario;
	private Entity entity;
	

	private int trace = Tracer.FINEST;
	private double testClock = 60.0;
	private double testEventTime = 60.0;

	// object variables used to control functionality of the move event for testing
	private int testLoop = 1;
	private double testCycleTime = 5.0;
	private int testMoverType = Constants.MOVER_TRACK;
	private Coordinate testObjective = new Coordinate (2.0, 2.0);
	private Coordinate testCurrentLocation = new Coordinate (1.0, 1.0);
	private int testElements = 1;
	private int testNonMovers = 0;
	private double testFuel = 100.000;
	private boolean testInoperative = false;
	private boolean testMounted = false;
	private boolean testMounting = false;
	private double testDelay = 0.0;
	private boolean testCBR = false;
	private int testCBRStatus = Constants.CBR_PACKED;
	private double testCBRTime = 300.0;
	private int testMoveMode = Constants.MOVE_NORMAL;
	private double testUploadTime = 0.0;
	private double testFiredLast = 0.0;
	private double testPackupTime = 0.0;
	private Entity testBridge = null;
	private double testGroupSpeed = 30.0;
	private double testSetSpeed = 100.0;
	private double testCcSpeed = 22.0;
	private double testRoadSpeed = 40.0;
	private double testCrawlSpeed = 5.0;
	private double testReverseSpeed = 15.0;
	private double testRunSped = 50.0;
	private double testSuppressionAmount = 0.0;
	private double testSuppressionSpeed = 0.5;
	private Flyer testFlier = null;
	private double testAltitude = 0.1;
	private boolean testRouting = false;
	private boolean testStopped = false;
	private double testHoldTime = 0.0;
	private boolean testPitting = false;
	private Route testRoute = new Route();
	private boolean testGUI = false;

	/**
	 * This class is executable.
	 * @param args
	 */
	public static void main(String[] args){
		TestMoveEventOld test = new TestMoveEventOld();
		test.doArgs(args);
		test.makeData();
		if (test.testGUI){
			test.runGUI();
		} else {
			Tracer.setEcho(true);
			test.test();
		}
	}
	
	public TestMoveEventOld(){
		myData = new CSData();
		scenario = new Scenario();
	}
	
	private void runGUI(){
		JFrame frame = new JFrame("testMoveEvent");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setPreferredSize(new Dimension(800, 600));
		frame.setMinimumSize(new Dimension(100,100));
		
		JTabbedPane tabbedPane = new JTabbedPane();
		
		TraceView view = new TraceView();
		
		tabbedPane.add("Output", view);
		tabbedPane.add("Map", new JPanel());
		tabbedPane.add("Scenario", new ScenarioViewOld(this.scenario));
		tabbedPane.add("Data", testView());
		
		frame.add(tabbedPane,BorderLayout.CENTER);
		frame.setVisible(true);
		frame.pack();
		frame.validate();
		
		Tracer.addListener(view);
		
//		Tracer.write("test");
		
		JPanel panel = new JPanel();
		JButton testBtn = new JButton("test");
		panel.add(testBtn);
		frame.add(panel,BorderLayout.SOUTH);
		testBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				test();
			}
		});
	}
	
	private JTextArea txtClock = new JTextArea("00:00:00:00");
	private JTextArea txtEventTime = new JTextArea("00:00:00:00");
	
	private JPanel testView(){
		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(0,6));
		panel.add(new JLabel("Trace "));
		panel.add(new JLabel("Clock "));
		txtClock.setText("" + testClock);
		panel.add(txtClock);
		panel.add(new JLabel("Event Time "));
		txtEventTime.setText("" + testEventTime);
		panel.add(txtEventTime);
		panel.add(new JLabel("Loop "));
		/*
	private int trace = Tracer.FINEST;
	private double testClock = 60.0;
	private double testEventTime = 60.0;

	// object variables used to control functionality of the move event for testing
	private int testLoop = 1;
	*/
		panel.add(new JLabel("Cycle Time "));
		panel.add(new JLabel("Mover Type "));
		panel.add(new JLabel("Objective "));
		panel.add(new JLabel("Location "));
		panel.add(new JLabel("Elements "));
		panel.add(new JLabel("Non-movers "));
		panel.add(new JLabel("Fuel "));
		panel.add(new JLabel("Inoperative "));
		panel.add(new JLabel("Mounted "));
		panel.add(new JLabel("Mounting "));
	/*
	private double testCycleTime = 5.0;
	private int testMoverType = Constants.MOVER_TRACK;
	private Coordinate testObjective = new Coordinate (2.0, 2.0);
	private Coordinate testCurrentLocation = new Coordinate (1.0, 1.0);
	private int testElements = 1;
	private int testNonMovers = 0;
	private double testFuel = 100.000;
	private boolean testInoperative = false;
	private boolean testMounted = false;
	private boolean testMounting = false;
	*/
		panel.add(new JLabel("Delay "));
		panel.add(new JLabel("CBR "));
		panel.add(new JLabel("CBR Status "));
		panel.add(new JLabel("CBR Time "));
		panel.add(new JLabel("Move Mode "));
		panel.add(new JLabel("Upload Time "));
		panel.add(new JLabel("Fired Last "));
		panel.add(new JLabel("Packup Time "));
		panel.add(new JLabel("Bridge "));
	/*
	private double testDelay = 0.0;
	private boolean testCBR = false;
	private int testCBRStatus = Constants.CBR_PACKED;
	private double testCBRTime = 300.0;
	private int testMoveMode = Constants.MOVE_NORMAL;
	private double testUploadTime = 0.0;
	private double testFiredLast = 0.0;
	private double testPackupTime = 0.0;
	private Entity testBridge = null;
	*/
		panel.add(new JLabel("Group Speed "));
		panel.add(new JLabel("Command Speed "));
		panel.add(new JLabel("CC Speed "));
		panel.add(new JLabel("Road Speed "));
		panel.add(new JLabel("Crawl Speed "));
		panel.add(new JLabel("Reverse Speed "));
		panel.add(new JLabel("Run Speed "));
		panel.add(new JLabel("Suppression Amount "));
		panel.add(new JLabel("Suppression Speed "));
	/*
	private double testGroupSpeed = 30.0;
	private double testSetSpeed = 100.0;
	private double testCcSpeed = 22.0;
	private double testRoadSpeed = 40.0;
	private double testCrawlSpeed = 5.0;
	private double testReverseSpeed = 15.0;
	private double testRunSped = 50.0;
	private double testSuppressionAmount = 0.0;
	private double testSuppressionSpeed = 0.5;
	*/
		panel.add(new JLabel("Flier "));
		panel.add(new JLabel("Altitude "));
		panel.add(new JLabel("Routing "));
		panel.add(new JLabel("Stopped "));
		panel.add(new JLabel("Hold Time "));
		panel.add(new JLabel("Pitting "));
		panel.add(new JLabel("Route "));
	/*
	private Flyer testFlier = null;
	private double testAltitude = 0.1;
	private boolean testRouting = false;
	private boolean testStopped = false;
	private double testHoldTime = 0.0;
	private boolean testPitting = false;
	private Route testRoute = new Route();
	private boolean testGUI = false;
		 * 
		 */
		return panel;
	}
	
	// test fire port see moveGround
	//test gotofloor see getObjective
	// test onBridge and waitingForBridge - see no move
	// test digging - see domove, with existing delay and without
	// stuff about buildings - see domove
	// check obstacles - esp craters see domove
	// test domove - already there
	// test already at location - domove
	// cc speed - domove
	// test speed = 0 (getspeed)
	
	// test terrain - cell size (domove)
	// what happens when you exit a pit crawling?
	
	private void makeData(){
		
//		Force force = new Force("force");
//		scenario.getForceList().add(force);
//		force.setSpeed(this.testGroupSpeed);
		
//		Platform platform = new Platform("test_platform");
//		myData.getPlatformList().add(platform);
//		Weapon weapon = new Weapon("test weapon");
//		myData.getWeaponList().add(weapon);
//		entity = new Entity("test_entity",platform);
//		scenario.getEntityList().add(entity);
		
		Entity e = (Entity) scenario.getEntityList().getFirst();
		if (e == null){
			Logger.err(Logger.SEVERE, "invalid entity");
			System.exit(0);
		}
		Force force = e.getForce();
		if (force == null){
			force = new Force("force");
			Logger.say("adding force ");
			e.setForce(force);
			scenario.getForceList().add(force);
		}
	}
	
	private void makeDataOld(){
		this.testDelay = Math.max(this.testDelay, this.testFiredLast+ this.testPackupTime);
		
		scenario.setClock(this.testClock);
		scenario.getParameters().setMovementCycleTime(this.testCycleTime);
		
		Force force = new Force("force");
		scenario.getForceList().add(force);
		force.setSpeed(this.testGroupSpeed);
		
		Platform platform = new Platform("test_platform");
		myData.getPlatformList().add(platform);
		
		platform.setMoverType(this.testMoverType);
		platform.setSpeedCountry(testCcSpeed);
		platform.setSpeedCrawling(testCrawlSpeed);
		platform.setSpeedReverse(testReverseSpeed);
		platform.setSpeedRoad(testRoadSpeed);
		platform.setSpeedRunning(testRunSped);
		platform.setSuppressionFactorMove(this.testSuppressionSpeed);
		if (this.testCBR) platform.setCBR(new CounterBatteryRadar());
		platform.setFlyerType(this.testFlier);

		Weapon weapon = new Weapon("test weapon");
		myData.getWeaponList().add(weapon);
		
		PlatformWeapon platformWeapon = new PlatformWeapon(weapon);
		platformWeapon.setPackupTime(this.testPackupTime);
		platform.addWeapon(platformWeapon);
		
		entity = new Entity("test_entity",platform);
		scenario.getEntityList().add(entity);
		
		EntityWeapon entityWeapon = entity.getWeaponList().get(0);
		entityWeapon.setFiredLast(this.testFiredLast);
		
		entity.setLocation(this.testCurrentLocation);
		entity.setMoveTo(this.testObjective);
		entity.setNumberOfElements(this.testElements);
		entity.setNonMovers(this.testNonMovers);
		entity.setCurrentFuel(this.testFuel);
		if (this.testInoperative){
			entity.setNBCInoperative(1); // TODO get actual code
		}
		entity.setDelay(this.testDelay);
		entity.setUploadTime(testUploadTime);
		entity.setSuppressionAmount(this.testSuppressionAmount);
		entity.setCBRStatus(this.testCBRStatus);
		entity.setCBRTime(this.testCBRTime);
		entity.setMoveMode(this.testMoveMode);
		entity.setCommandSpeed(this.testSetSpeed);
		entity.setAltitude(this.testAltitude);
		entity.enterBridge(this.testBridge);
		if (testStopped) entity.stop();
		if (testHoldTime > 0){
			entity.setHold(testHoldTime);
		}
		if ( this.testMounted){
			Entity carrier = new Entity("carrier",platform);
			carrier.setLocation(this.testCurrentLocation);
			entity.mount(carrier);
		}
		if (this.testMounting){
			Entity carrier = new Entity("carrier to mount",platform);
			carrier.setLocation(
					new Coordinate(this.testCurrentLocation.getX()+0.5,
							this.testCurrentLocation.getY() + 0.5));
			entity.setCommandMount(carrier);
		}
		if (this.testPitting){ // TODO add pit coords to args
			Pit pit = new Pit();
			pit.setLocation(
					new Coordinate(this.testCurrentLocation.getX()+0.01,
							this.testCurrentLocation.getY() + 0.01));
			entity.setCommandPit(pit);
		}
		if (this.testRouting){
			entity.setRoute(testRoute);
		}
		
	}

	/**
	 * Run a test of the class MoveEvent
	 */
	private void test(){
		Tracer.setLevel(this.trace);
		Tracer.write(Tracer.MOVEMENT, 0, "Testing class MoveEvent");
		
		listData(); //TODO should use actual values from scenario etc

		scenario.setClock(scenario.getParameters().getStartTime());
		for (int i=0;i< scenario.getEntityList().getSize();i++){
			/* FIXME
			Entity e = scenario.getEntityList().getEntity(i);
			MoveEvent event = new MoveEvent(this.testEventTime, scenario, e);
			event.setTime(scenario.getParameters().getStartTime());
			event.doEvent();
			*/
		}
		
		/*
		MoveEvent event = new MoveEvent(this.testEventTime, scenario, entity);
		event.setTime(this.testEventTime);
		for (int i=0;i<testLoop;i++){
			event.doEvent();
			scenario.setClock(scenario.getClock()+ this.testCycleTime);
		}
		*/
	}

	/**
	 * List all of the parameters that control the functionality of the 
	 * MoveEvent test. 
	 */
	private void listData(){
		Tracer.write(Tracer.MOVEMENT, 1, "data values used in the test:");
		Tracer.write("listing platforms");
		Tracer.write("listing entities");
		Tracer.write(scenario.getEntityList().listDetails());
		/*
		Tracer.write(Tracer.MOVEMENT, 1, "game clock " + testClock);
		Tracer.write(Tracer.MOVEMENT, 1, "event time " + testEventTime);
		Tracer.write(Tracer.MOVEMENT, 1, "loop " + testLoop);
		Tracer.write(Tracer.MOVEMENT, 1, "move cycle time " + testCycleTime);
		Tracer.write(Tracer.MOVEMENT, 1, "mover type " + testMoverType);
		Tracer.write(Tracer.MOVEMENT, 1, "flier type " + testFlier);
		Tracer.write(Tracer.MOVEMENT, 1, "number of elements " + testElements);
		Tracer.write(Tracer.MOVEMENT, 1, "imobile elements " + testNonMovers);
		Tracer.write(Tracer.MOVEMENT, 1, "inoperative due to NBC " + testInoperative);
		Tracer.write(Tracer.MOVEMENT, 1, "stopped " + testStopped);
		Tracer.write(Tracer.MOVEMENT, 1, "hold time " + testHoldTime);
		
		Tracer.write(Tracer.MOVEMENT, 1, "fuel " + testFuel);
		Tracer.write(Tracer.MOVEMENT, 1, "move mode " + testMoveMode);
		Tracer.write(Tracer.MOVEMENT, 1, "delay time " + testDelay);
		Tracer.write(Tracer.MOVEMENT, 1, "upload time " + testUploadTime);
		Tracer.write(Tracer.MOVEMENT, 1, "packup time " + testPackupTime);
		Tracer.write(Tracer.MOVEMENT, 1, "last fired time " + testFiredLast);
		Tracer.write(Tracer.MOVEMENT, 1, "group speed " + testGroupSpeed);
		Tracer.write(Tracer.MOVEMENT, 1, "road speed " + testRoadSpeed);
		Tracer.write(Tracer.MOVEMENT, 1, "run speed " + testRunSped);
		Tracer.write(Tracer.MOVEMENT, 1, "cross country speed " + testCcSpeed);
		Tracer.write(Tracer.MOVEMENT, 1, "crawl speed " + testCrawlSpeed);
		Tracer.write(Tracer.MOVEMENT, 1, "reverse speed " + testReverseSpeed);
		Tracer.write(Tracer.MOVEMENT, 1, "mounted " + testMounted);
		Tracer.write(Tracer.MOVEMENT, 1, "CBR " + testCBR);
		Tracer.write(Tracer.MOVEMENT, 1, "CBR Status " + testCBRStatus);
		Tracer.write(Tracer.MOVEMENT, 1, "CBR Time " + testCBRTime);
		Tracer.write(Tracer.MOVEMENT, 1, "test bridge " + testBridge);
		Tracer.write(Tracer.MOVEMENT, 1, "suppression amount " + testSuppressionAmount);
		Tracer.write(Tracer.MOVEMENT, 1, "supression speed factor " + testSuppressionSpeed);
		
		Tracer.write(Tracer.MOVEMENT, 1, "starting location " + testCurrentLocation);
		Tracer.write(Tracer.MOVEMENT, 1, "starting altitude " + testAltitude);
		Tracer.write(Tracer.MOVEMENT, 1, "move to objective " + testObjective);
		Tracer.write(Tracer.MOVEMENT, 1, "mounting " + testMounting);
		Tracer.write(Tracer.MOVEMENT, 1, "following a route " + testRouting);
		Tracer.write(Tracer.MOVEMENT, 1, "mounting a pit " + testPitting);
		*/
	}
	
	/**
	 * Read the command line arguments and set the test parameters accordingly.
	 * @param args a list of command line parameters.
	 */
	private void doArgs(String[] args){
		for (int i = 0;i<args.length;i++){
			String s = args[i];
			if (s.compareToIgnoreCase("-")==0){
			} else if (s.compareToIgnoreCase("-advance")==0){
				this.testMoveMode = Constants.MOVE_ADVANCE;
			} else if (s.compareToIgnoreCase("-assault")==0){
				this.testMoveMode = Constants.MOVE_ASSAULT;
			} else if (s.compareToIgnoreCase("-bridge")==0){
				this.testBridge = new Entity("bridge entity", 
						new Platform("bridge platform"));
			} else if (s.compareToIgnoreCase("-cautious")==0){
				this.testMoveMode = Constants.MOVE_CAUTIOUS;
			} else if (s.compareToIgnoreCase("-cbr")==0){
				this.testCBR = true;
				i++;
				s = args[i];
				if (s.compareToIgnoreCase("on")==0){
					this.testCBRStatus = Constants.CBR_ON;
				} else if (s.compareToIgnoreCase("setup")==0){
					this.testCBRStatus = Constants.CBR_SETUP;
				} else if (s.compareToIgnoreCase("packed")==0){
					this.testCBRStatus = Constants.CBR_PACKED;
				}
			} else if (s.compareToIgnoreCase("-cbrtime")==0){
				i++;
				try{
					this.testCBRTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid cbr time");
				}
			} else if (s.compareToIgnoreCase("-clock")==0){
				i++;
				try{
					this.testClock = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid clock");
				}
			} else if (s.compareToIgnoreCase("-crawl")==0){
				this.testMoveMode = Constants.MOVE_CRAWL;
			} else if (s.compareToIgnoreCase("-crawlspeed")==0){
				i++;
				try{
					this.testCrawlSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid crawl speed");
				}
			} else if (s.compareToIgnoreCase("-cycle")==0){
				i++;
				try{
					this.testCycleTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid cycle time");
				}
			} else if (s.compareToIgnoreCase("-delay")==0){
				i++;
				try{
					this.testDelay = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid delay");
				}
			} else if (s.compareToIgnoreCase("-elements")==0){
				i++;
				try{
					this.testElements = Integer.parseInt(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid number of elements");
				}
			} else if (s.compareToIgnoreCase("-file")==0){
				i++;
				String fileName = args[i];
				CommandInterpreter interpreter = new CommandInterpreter(myData, scenario);
				interpreter.interpret("load " + fileName);
			} else if (s.compareToIgnoreCase("-fired")==0){
				i++;
				try{
					this.testFiredLast = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid fired time");
				}
			} else if (s.compareToIgnoreCase("-flier")==0){
				this.testFlier = new Flyer();
			} else if (s.compareToIgnoreCase("-foot")==0){
				this.testMoveMode = Constants.MOVER_FOOT;
			} else if (s.compareToIgnoreCase("-fuel")==0){
				i++;
				try{
					this.testFuel = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid fuel");
				}
			} else if (s.compareToIgnoreCase("-group")==0){
				this.testMoveMode = Constants.MOVE_GROUP;
			} else if (s.compareToIgnoreCase("-groupspeed")==0){
				i++;
				try{
					this.testGroupSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid group speed");
				}
			} else if (s.compareToIgnoreCase("-gui")==0){
				this.testGUI = true;
			} else if (s.compareToIgnoreCase("-inoperative")==0){
				this.testInoperative = true;
			} else if (s.compareToIgnoreCase("-location")==0){
				try{
					i++;
					double x = Double.parseDouble(args[i]);
					i++;
					double y = Double.parseDouble(args[i]);
					this.testCurrentLocation = new Coordinate(x,y);
				} catch (Exception e){
					Logger.err(this,0, "invalid location");
				}
			} else if (s.compareToIgnoreCase("-loop")==0){
				i++;
				try{
					this.testLoop = Integer.parseInt(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid loop counter");
				}
			} else if (s.compareToIgnoreCase("-mounted")==0){
				this.testMounted = true;
			} else if (s.compareToIgnoreCase("-mounting")==0){
				this.testMounting = true;
			} else if (s.compareToIgnoreCase("-moveto")==0){
				i++;
				s = args[i];
				if (s.compareToIgnoreCase("null")==0){
					this.testObjective = null;
				} else {
					try{
						double x = Double.parseDouble(args[i]);
						i++;
						double y = Double.parseDouble(args[i]);
						this.testObjective = new Coordinate(x,y);
					} catch (Exception e){
						Logger.err(this,0, "invalid objective");
					}
				}
			} else if (s.compareToIgnoreCase("-node")==0){
				try{
					i++;
					s = args[i];
					i++;
					double x = Double.parseDouble(args[i]);
					i++;
					double y = Double.parseDouble(args[i]);
					if (s.compareToIgnoreCase("go")==0){
						NodeGo n = new NodeGo();
						n.setLocation(new Coordinate(x,y));
						testRoute.add(n);
					} else if (s.compareToIgnoreCase("hold")==0){
						NodeHold n = new NodeHold();
						n.setLocation(new Coordinate(x,y));
						i++;
						double time = Double.parseDouble(args[i]);
						n.setTime(time);
						testRoute.add(n);
					} else if (s.compareToIgnoreCase("los")==0){
						NodeLOS n = new NodeLOS();
						n.setLocation(new Coordinate(x,y));
						testRoute.add(n);
					} else if (s.compareToIgnoreCase("stop")==0){
						NodeStop n = new NodeStop();
						n.setLocation(new Coordinate(x,y));
						testRoute.add(n);
					}
				} catch (Exception e){
					Logger.err(this,0, "invalid node data");
				}
			} else if (s.compareToIgnoreCase("-nonmovers")==0){
				i++;
				try{
					this.testNonMovers = Integer.parseInt(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid number of nonmovers");
				}
			} else if (s.compareToIgnoreCase("-normal")==0){
				this.testMoveMode = Constants.MOVE_NORMAL;
			} else if (s.compareToIgnoreCase("-packup")==0){
				i++;
				try{
					this.testPackupTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid packup time");
				}
			} else if (s.compareToIgnoreCase("-pit")==0){
				this.testPitting = true;
			} else if (s.compareToIgnoreCase("-reverse")==0){
				this.testMoveMode = Constants.MOVE_REVERSE;
			} else if (s.compareToIgnoreCase("-reversespeed")==0){
				i++;
				try{
					this.testReverseSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid reverse speed");
				}
			} else if (s.compareToIgnoreCase("-route")==0){
				this.testRouting = true;
			} else if (s.compareToIgnoreCase("-run")==0){
				this.testMoveMode = Constants.MOVE_RUN;
			} else if (s.compareToIgnoreCase("-runspeed")==0){
				i++;
				try{
					this.testRunSped = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid run speed");
				}
			} else if (s.compareToIgnoreCase("-rush")==0){
				this.testMoveMode = Constants.MOVE_RUSH;
			} else if (s.compareToIgnoreCase("-slow")==0){
				this.testMoveMode = Constants.MOVE_SLOW;
				i++;
				try{
					this.testSetSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid speed");
				}
			} else if (s.compareToIgnoreCase("-speed")==0){
				i++;
				try{
					this.testCcSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid speed");
				}
			} else if (s.compareToIgnoreCase("-stopped")==0){
				this.testStopped = true;
			} else if (s.compareToIgnoreCase("-suppression")==0){
				i++;
				try{
					this.testSuppressionAmount = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid suppression value");
				}
			} else if (s.compareToIgnoreCase("-suppressionspeed")==0){
				i++;
				try{
					this.testSuppressionSpeed = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid suppression speed");
				}
			} else if (s.compareToIgnoreCase("-time")==0){
				i++;
				try{
					this.testEventTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid event time");
				}
			} else if (s.compareToIgnoreCase("-trace")==0){
				i++;
				s = args[i];
				if (s.compareToIgnoreCase("coursest")==0){
					this.trace = Tracer.COURSEST;
				} else if (s.compareToIgnoreCase("courser")==0){
					this.trace = Tracer.COURSER;
				} else if (s.compareToIgnoreCase("course")==0){
					this.trace = Tracer.COURSE;
				} else if (s.compareToIgnoreCase("fine")==0){
					this.trace = Tracer.FINE;
				} else if (s.compareToIgnoreCase("finer")==0){
					this.trace = Tracer.FINER;
				} else if (s.compareToIgnoreCase("finest")==0){
					this.trace = Tracer.FINEST;
				} else {
					Logger.err(this,0, "invalid trace option");
				}
			} else if (s.compareToIgnoreCase("-track")==0){
				this.testMoveMode = Constants.MOVER_TRACK;
			} else if (s.compareToIgnoreCase("-upload")==0){
				i++;
				try{
					this.testUploadTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid upload time");
				}
			} else if (s.compareToIgnoreCase("-wheel")==0){
				this.testMoveMode = Constants.MOVER_WHEEL;
			} else {
				Logger.err(this,0, "invalid parameter " + s);
			}
		}
	}
	
}
