package interpreter;

import java.util.Vector;

import data.csd.Sensor;
import data.map.Coordinate;
import sim.Constants;
import sim.Scenario;
import sim.entity.Entity;
import sim.forces.Force;
import sim.route.Node;
import sim.route.NodeGo;
import sim.route.NodeHold;
import sim.route.NodeLOS;
import sim.route.NodeStop;
import utils.Logger;
import utils.Parser;
import utils.Tracer;

public class EntityInterpreter extends Interpreter{
	private Entity myEntity;
	private Scenario myScenario;
	
	public void doCommand(Scenario scenario, Entity entity, String command, Vector<String> vector){
		if (entity == null) return;
		if (scenario == null) return;
		myEntity = entity;
		myScenario = scenario;
		doCommand(command, vector);
		myEntity = null;
		myScenario = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myEntity == null) return;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("add_node") == 0){
			Node node = makeNode(vector);
			if (node == null) return;
			myEntity.addNode(node);
		} else if (command.compareToIgnoreCase("advance")==0){
			myEntity.setMoveMode(Constants.MOVE_ADVANCE);
		} else if (command.compareToIgnoreCase("assault")==0){
			myEntity.setMoveMode(Constants.MOVE_ASSAULT);
		} else if (command.compareToIgnoreCase("carrier") == 0){
			if (vector.isEmpty()) return;
			String s = vector.remove(0);
			Entity carrier = myScenario.getEntityList().getEntity(s);
			if (carrier == null) return;
			myEntity.SetDefaultCarrier(carrier);
		} else if (command.compareToIgnoreCase("cautious")==0){
			myEntity.setMoveMode(Constants.MOVE_CAUTIOUS);
		} else if (command.compareToIgnoreCase("cbr")==0){
			if (vector.isEmpty()) return;
			String s = vector.remove(0);
			if (s.compareToIgnoreCase("on")==0){
				myEntity.setCBRStatus(Constants.CBR_ON);
			} else if (s.compareToIgnoreCase("setup")==0){
				myEntity.setCBRStatus(Constants.CBR_SETUP);
			} else if (s.compareToIgnoreCase("packed")==0){
				myEntity.setCBRStatus(Constants.CBR_PACKED);
			}
		} else if (command.compareToIgnoreCase("cbrtime")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0));
				myEntity.setCBRTime(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid cbr time");
			}
		} else if (command.compareToIgnoreCase("circle") == 0){
			if (vector.size()<2) return;
			Coordinate c = Parser.parseCoordinate(myEntity.getLocation(), 
					vector.remove(0), vector.remove(0));
			if (c == null) return;
			myEntity.setCircle(c);
		} else if (command.compareToIgnoreCase("circle_time")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0));
				myEntity.setCircleTime(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid circle time");
			}
		} else if (command.compareToIgnoreCase("command_speed")==0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0));
				myEntity.setMoveMode(Constants.MOVE_SLOW);
				myEntity.setCommandSpeed(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid speed");
			}
		} else if (command.compareToIgnoreCase("crawl")==0){
			myEntity.setMoveMode(Constants.MOVE_CRAWL);
		} else if (command.compareToIgnoreCase("delay")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0)); 
				myEntity.setDelay(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid delay");
			}
		} else if (command.compareToIgnoreCase("elements")==0){
			if (vector.isEmpty()) return;
			String string = vector.remove(0);
			try{
				int i = Integer.parseInt(string);
				myEntity.setNumberOfElements(i);
			} catch (Exception e){
				Logger.err(this,0, "invalid number of elements");
			}
		} else if (command.compareToIgnoreCase("fired")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0)); 
				myEntity.setLastFire(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid fired time");
			}
		} else if (command.compareToIgnoreCase("force") == 0){
			if (vector.isEmpty()) return;
			String forceName = vector.remove(0);
			Force force = myScenario.getForceList().getForce(forceName);
			if (force == null) return; //TODO could warn
			myEntity.setForce(force);
		} else if (command.compareToIgnoreCase("fuel")==0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myEntity.setCurrentFuel(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid fuel");
			}
		} else if (command.compareToIgnoreCase("hold")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0)); 
				myEntity.setHold(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid hold time");
			}
		} else if (command.compareToIgnoreCase("group")==0){
			myEntity.setMoveMode(Constants.MOVE_GROUP);
		} else if (command.compareToIgnoreCase("inoperative")==0){
			if (vector.isEmpty()) {
				myEntity.setNBCInoperative(Constants.INOPERATIVE_ATROPINE);
				return;
			}
			String s = vector.remove(0);
			if (s.compareToIgnoreCase("atropine")==0){
				myEntity.setNBCInoperative(Constants.INOPERATIVE_ATROPINE);
			} else if (s.compareToIgnoreCase("control")==0){
				myEntity.setNBCInoperative(Constants.INOPERATIVE_LOSS_OF_CONTROL);
			} else if (s.compareToIgnoreCase("dose")==0){
				myEntity.setNBCInoperative(Constants.INOPERATIVE_LETHAL_DOSE);
			} else if (s.compareToIgnoreCase("detector")==0){
				myEntity.setNBCInoperative(Constants.INOPERATIVE_DETECTOR_SET);
			} else if (s.compareToIgnoreCase("heat")==0){
				myEntity.setNBCInoperative(Constants.INOPERATIVE_HEAT);
			} else if (s.compareToIgnoreCase("no")==0){
				myEntity.setNBCInoperative(0);
			}
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		} else if (command.compareToIgnoreCase("location") == 0){
			if (vector.size()<2) return;
			Coordinate c = Parser.parseCoordinate(myEntity.getLocation(), 
					vector.remove(0), vector.remove(0));
			if (c == null) return;
			myEntity.setLocation(c);
		} else if (command.compareToIgnoreCase("move_to") == 0){
			if (vector.size()<2) return;
			Coordinate c = Parser.parseCoordinate(myEntity.getLocation(), 
					vector.remove(0), vector.remove(0));
			if (c == null) return;
			myEntity.setMoveTo(c);
		} else if (command.compareToIgnoreCase("mount") == 0){
			if (vector.isEmpty()) {
				myEntity.mount();
			} else {
				String s = vector.remove(0);
				Entity carrier = myScenario.getEntityList().getEntity(s);
				if ( carrier == null) return;
				if (myScenario.isRunning()){
					myEntity.setCommandMount(carrier);
				} else {
					myEntity.mount(carrier);
				}
			}
		} else if (command.compareToIgnoreCase("nonmovers")==0){
			if (vector.size()<1) return;
			try{
				int i = Integer.parseInt(vector.remove(0));
				myEntity.setNonMovers(i);
			} catch (Exception e){
				Logger.err(this,0, "invalid number of nonmovers");
			}
		} else if (command.compareToIgnoreCase("normal")==0){
			myEntity.setMoveMode(Constants.MOVE_NORMAL);
		} else if (command.compareToIgnoreCase("radius")==0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				if (d < 0.0) return;
				myEntity.setRadius(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid radius");
			}
		} else if (command.compareToIgnoreCase("reverse")==0){
			myEntity.setMoveMode(Constants.MOVE_REVERSE);
		} else if (command.compareToIgnoreCase("run")==0){
			myEntity.setMoveMode(Constants.MOVE_RUN);
		} else if (command.compareToIgnoreCase("rush")==0){
			myEntity.setMoveMode(Constants.MOVE_RUSH);
		} else if (command.compareToIgnoreCase("stop")==0){
			myEntity.stop();
		} else if (command.compareToIgnoreCase("sensor") == 0){ //TODO could ue a number too?
			if (vector.isEmpty()) return;
			String sensorName = vector.remove(0);
			Sensor sensor = myEntity.getPlatform().getSensorList().getSensor(sensorName);
			if (sensor == null) return;
			myEntity.setCurrentSensor(sensor);
		} else if (command.compareToIgnoreCase("suppression")==0){
			if (vector.size()<1) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myEntity.setSuppressionAmount(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid suppression value");
			}
		} else if (command.compareToIgnoreCase("trace") == 0){
			if (vector.isEmpty()) return;
			String string = vector.remove(0);
			if (string.compareToIgnoreCase("on")== 0){
				myEntity.setTracing(true);
			} else if (string.compareToIgnoreCase("off")== 0){
				myEntity.setTracing(false);
			}
		} else if (command.compareToIgnoreCase("upload_time")==0){
			if (vector.isEmpty()) return;
			try{
				double time = Parser.parseTime(myScenario.getClock(), 
						vector.remove(0)); 
				myEntity.setUploadTime(time);
			} catch (Exception e){
				Logger.err(this,0, "invalid upload time");
			}
		} else {
			Logger.err(this,Logger.WARNING, "invalid command " + command);
		}
	}
	
	private Node makeNode (Vector<String> vector){ 
		if (vector.size()<2) return null;
		Coordinate c = Parser.parseCoordinate(myEntity.getLocation(), 
				vector.remove(0), vector.remove(0));
		if ( c == null) return null;
		Node node = new Node();
		if (vector.isEmpty()) {
			node = new NodeGo();
		} else {
			String type = vector.remove(0);
			if (type.compareToIgnoreCase("") == 0){
				/* TODO more node types
			 * NodeAGL
			 * NodeAMSL
			 * NodeAreaFire
			 * NodeAreaWeapon
			 * NodeAssault
			 * NodeBridge
			 * NodeCircle
			 * NodeClearMines
			 * NodeCommandWeapon
			 * NodeContactSOP
			 * NodeCrawl
			 * NodeDismount
			 * NodeDown
			 * NodeEngagedSOP
			 * NodeExhaust
			 * NodeFirePort
			 * NodeFloor
			 * NodeFormation
			 * NodeGrenade
				 */
			} else if (type.compareToIgnoreCase("go") == 0){
				node = new NodeGo();
			} else if (type.compareToIgnoreCase("hold") == 0){
				NodeHold n = new NodeHold();
				double d;
				if (vector.isEmpty()){
					d = 0.0;
				} else {
					d = Double.parseDouble(vector.remove(0));
				}
				n.setTime(d);
				node = n;
			} else if (type.compareToIgnoreCase("los")==0){
				node = new NodeLOS();
			} else if (type.compareToIgnoreCase("stop")==0){
				node = new NodeStop();
			}
			/*
			 * NodeLayMines
			 * NodeLineCharge
			 * NodeLogWeapon
			 * NodeMineSOP
			 * NodeMount
			 * NodeObstacle
			 * NodeOnTrigger
			 * NodePit
			 * NodeRelease
			 * NodeReverse
			 * NodeROE
			 * NodeSensor
			 * NodeSpeed
			 * NodeTrigger
			 * NodeUp
			 * NodeWaterSOP
			 */
			else {
				return null;
			}
		}
		node.setLocation(c);
		return node;

	}

	

	/*
	 * 
			} else if (s.compareToIgnoreCase("-bridge")==0){
				this.testBridge = new Entity("bridge entity", 
						new Platform("bridge platform"));
			} else if (s.compareToIgnoreCase("-flier")==0){
				this.testFlier = new Flyer();
			} else if (s.compareToIgnoreCase("-gui")==0){
				this.testGUI = true;
			} else if (s.compareToIgnoreCase("-packup")==0){
				i++;
				try{
					this.testPackupTime = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(this,0, "invalid packup time");
				}
			} else if (s.compareToIgnoreCase("-pit")==0){
				this.testPitting = true;
			} else {
				Logger.err(this,0, "invalid parameter " + s);
			}
	 */
	
}
