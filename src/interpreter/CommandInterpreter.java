package interpreter;

import java.util.Vector;

import data.CSData;
import data.csd.Platform;
import data.csd.Sensor;
import data.csd.Weapon;
import data.map.Map;
import sim.GameClock;
import sim.Scenario;
import sim.Simulation;
import sim.entity.Entity;
import sim.forces.Force;
import utils.Logger; //TODO make logging optional
import utils.Tracer;

public class CommandInterpreter extends Interpreter{
	private Simulation mySimulation;
	
	private CSData myData;
	private Scenario myScenario;
	private GameClock gameClock;
	private EntityInterpreter myEntityInterpreter;
	private PlatformInterpreter myPlatformInterpreter;
	private ScenarioInterpreter myScenarioInterpreter;
	private ForceInterpreter myForceInterpreter;
	private SensorInterpreter mySensorInterpreter;
	private WeaponInterpreter myWeaponInterpreter;
	private MapInterpreter myMapInterpreter;
	
	public static void main(String args[]){
		Simulation simulation = new Simulation();
		CommandInterpreter me = new CommandInterpreter(simulation);
		me.interpret(args);
//		myData.log();
	}
	
	public CommandInterpreter(Simulation simulation){
		super();
		if (simulation == null){
			Logger.err(Logger.WARNING, "CommandInterpreter: invalid data");
			return;
		}
		mySimulation = simulation;
		myData = simulation.getCSData();
		if (myData == null){
			Logger.err(Logger.WARNING, "CommandInterpreter: invalid data");
			return;
		}
		myScenario = simulation.getScenario();
		if (myScenario == null){
			Logger.err(Logger.WARNING, "CommandInterpreter: invalid scenario");
			return;
		}
		gameClock = simulation.getGameClock();

		myEntityInterpreter = new EntityInterpreter();
		myPlatformInterpreter = new PlatformInterpreter(myData);
		myScenarioInterpreter = new ScenarioInterpreter();
		myForceInterpreter = new ForceInterpreter();
		mySensorInterpreter = new SensorInterpreter();
		myWeaponInterpreter = new WeaponInterpreter();
		myMapInterpreter = new MapInterpreter();
	}

	public void setTrace(boolean b){
		trace = b;
		this.myEntityInterpreter.setTrace(b);
		this.myPlatformInterpreter.setTrace(b);
		this.myScenarioInterpreter.setTrace(b);
		this.myForceInterpreter.setTrace(b);
	}
	
	
	protected void doCommand(String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myData == null) {
			return;
		}
		if (myScenario == null) return;
		if (vector.isEmpty()) return;
		if (command.compareToIgnoreCase("")==0){
		} else if (command.compareToIgnoreCase("entity")== 0){
			doEntity(vector);
		} else if (command.compareToIgnoreCase("force")== 0){
			doForce(vector);
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			Logger.err(Logger.INFO, "loading file " + fileName);
			read(fileName);
		} else if (command.compareToIgnoreCase("new")== 0){
			doNew(vector);
		} else if (command.compareToIgnoreCase("platform")== 0){
			doPlatform(vector);
		} else if (command.compareToIgnoreCase("scenario")== 0){
			doScenario(vector);
		} else if (command.compareToIgnoreCase("sensor")== 0){
			doSensor(vector);
		} else if (command.compareToIgnoreCase("trace")== 0){
			doTrace(vector);
		} else if (command.compareToIgnoreCase("weapon")== 0){
			doWeapon(vector);
		} else if (command.compareToIgnoreCase("map")== 0){
			doMap(vector);
		} else {
			Logger.log("Commandinterpreter: unknown command: " + command + vector);
		}
	}
	
	//TODO add weapon and sensor
	private void doNew(Vector<String> vector){
		if (vector.isEmpty()) return;
		String command = vector.remove(0);
		if (command.compareToIgnoreCase("")==0){
		} else if (command.compareToIgnoreCase("entity")==0){
			String name = vector.remove(0);
			String platformName = vector.remove(0);
			if (trace){
				Logger.log("creating new entity " + name + ":" + platformName);
			}
			Platform platform = myData.getPlatformList().getPlatform(platformName);
			if (platform == null) {
				Logger.err(Logger.WARNING, "platform " + platformName + " does not exist");
				return;
			}
			Entity entity = new Entity(name, platform);
			if (!myScenario.getEntityList().add(entity)) {
				Logger.err(Logger.WARNING, "unable to add entity " + name);
			}
		} else if (command.compareToIgnoreCase("force")==0){
			String name = vector.remove(0);
			if (trace){
				Logger.log("creating new force " + name);
			}
			myScenario.addForce(name);
		} else if (command.compareToIgnoreCase("platform")==0){
			String name = vector.remove(0);
			if (trace){
				Logger.log("creating new platform " + name);
			}
			Platform platform = new Platform(name);
			myData.getPlatformList().add(platform);
		} else if (command.compareToIgnoreCase("sensor")==0){
			String name = vector.remove(0);
			if (trace){
				Logger.log("creating new sensor " + name); //FIXME
			}
			Sensor sensor = new Sensor(name);
			myData.getSensorList().add(sensor);
		} else if (command.compareToIgnoreCase("weapon")==0){
			String name = vector.remove(0);
			if (trace){
				Logger.log("creating new weapon " + name); //FIXME
			}
			Platform platform = new Platform(name);
			myData.getPlatformList().add(platform);
		}
		
	}

	private void doEntity(Vector<String> vector){
		if (vector.size()<2) return;
		String name = vector.remove(0);
		Entity entity = myScenario.getEntityList().getEntity(name);
		if (entity == null) return;
		String command = vector.remove(0);
		myEntityInterpreter.doCommand(myScenario, gameClock, entity, command, vector);
	}

	private void doForce(Vector<String> vector){
		if (vector.size()<2) return;
		String name = vector.remove(0);
		Force force = myScenario.getForceList().getForce(name);
		if (force == null) return;
		String command = vector.remove(0);
		myForceInterpreter.doCommand(force, command, vector);
	}

	private void doPlatform(Vector<String> vector){
		if (vector.size()<2) return;
		String name = vector.remove(0);
		Platform platform = myData.getPlatformList().getPlatform(name);
		if (platform == null) return;
		String command = vector.remove(0);
		myPlatformInterpreter.doCommand(platform, command, vector);
	}

	private void doSensor(Vector<String> vector){
		if (vector.size()<2) return;
		String name = vector.remove(0);
		Sensor sensor = myData.getSensorList().getSensor(name);
		if (sensor == null) return;
		String command = vector.remove(0);
		mySensorInterpreter.doCommand(sensor, command, vector);
	}

	private void doWeapon(Vector<String> vector){
		if (vector.size()<2) return;
		String name = vector.remove(0);
		Weapon weapon = myData.getWeaponList().getWeapon(name);
		if (weapon == null) return;
		String command = vector.remove(0);
		myWeaponInterpreter.doCommand(weapon, command, vector);
	}

	private void doScenario(Vector<String> vector){
		if (vector.size()<1) return;
		String command = vector.remove(0);
		myScenarioInterpreter.doCommand(myScenario, gameClock, command, vector);
	}

	private void doMap(Vector<String> vector){
		if (vector.size()<2) return;
		Map map = myScenario.getMap();
		if (map == null) return;
		String command = vector.remove(0);
		myMapInterpreter.doCommand(map, command, vector);
	}

	private void doTrace(Vector<String> vector){
		if (vector.size()<1) return;
		String command = vector.remove(0);
		if (command.compareToIgnoreCase("")==0){
		} else if (command.compareToIgnoreCase("movement")== 0){
			if (vector.size()<1) return;
			String option = vector.remove(0);
			if (option.compareToIgnoreCase("")==0){
			} else if (option.compareToIgnoreCase("on")== 0){
				Tracer.setMovement(true);
			} else if (option.compareToIgnoreCase("off")== 0){
				Tracer.setMovement(false);
			}
		}
	}

}
