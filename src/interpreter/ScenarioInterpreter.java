package interpreter;

import java.util.Vector;

import sim.Constants;
import sim.GameClock;
import sim.Scenario;
import utils.Logger;
import utils.Tracer;

public class ScenarioInterpreter extends Interpreter{
	private Scenario myScenario;
	private GameClock gameClock;
	
	public void doCommand(Scenario scenario, GameClock clock, String command, Vector<String> vector){
		if (scenario == null) return;
		myScenario = scenario;
		gameClock = clock;
		doCommand(command, vector);
		myScenario = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myScenario == null) return;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("casualty_cycle") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg);
				myScenario.getParameters().setCasualtyCycleTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid casualty_cycle");
			}
		} else if (command.compareToIgnoreCase("checkpoint") == 0){
			String arg = vector.remove(0);
			try{
				double value = Double.parseDouble(arg);
				myScenario.getParameters().setCheckPointFrequency(value);
			} catch (Exception e){
				Logger.err(this,0, "invalid checkpoint time");
			}
		} else if (command.compareToIgnoreCase("defilade_time") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg); //TODO parse time
				myScenario.getParameters().setDefiladeTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid defilade_time");
			}
		} else if (command.compareToIgnoreCase("detect_friends") == 0){
			if (vector.isEmpty()) return;
			String arg = vector.remove(0);
			if (arg.compareToIgnoreCase("no")==0){
				myScenario.getParameters().setDoSeeOwn(Constants.DETECT_FRIENDS_NEVER);
			} else if (arg.compareToIgnoreCase("yes")==0){
				myScenario.getParameters().setDoSeeOwn(Constants.DETECT_FRIENDS_WHEN_SEEN);
			} else if (arg.compareToIgnoreCase("always")==0){
				myScenario.getParameters().setDoSeeOwn(Constants.DETECT_FRIENDS_ALWAYS);
			}
		} else if (command.compareToIgnoreCase("dismount_inner") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangeDismountInner(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid dismount_inner");
			}
		} else if (command.compareToIgnoreCase("dismount_outer") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangeDismountOuter(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid dismount_outer");
			}
		} else if (command.compareToIgnoreCase("end_time") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg); //TODO parse time
				myScenario.getParameters().setEndTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid end_time");
			}
		} else if (command.compareToIgnoreCase("file") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		} else if (command.compareToIgnoreCase("fire_cycle") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg);
				myScenario.getParameters().setDirectFireCycleTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid fire_cycle");
			}
		} else if (command.compareToIgnoreCase("map") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			myScenario.getParameters().setMapFileName(fileName);
		} else if (command.compareToIgnoreCase("mount_inner") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangeMountInner(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid mount_inner");
			}
		} else if (command.compareToIgnoreCase("mount_outer") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangeMountOuter(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid mount_outer");
			}
		} else if (command.compareToIgnoreCase("move_cycle") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg);
				myScenario.getParameters().setMovementCycleTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid move_cycle");
			}
		} else if (command.compareToIgnoreCase("name") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			myScenario.setName(fileName);
		} else if (command.compareToIgnoreCase("nbc_speed") == 0){
			String arg = vector.remove(0);
			try{
				double value = Double.parseDouble(arg);
				myScenario.getParameters().setSpeedMOPP(value);
			} catch (Exception e){
				Logger.err(this,0, "invalid nbc_speed");
			}
		} else if (command.compareToIgnoreCase("partial_kills") == 0){
			if (vector.isEmpty()) return;
			String arg = vector.remove(0);
			if (arg.compareToIgnoreCase("no")==0){
				myScenario.getParameters().setPartialKills(false);
			} else if (arg.compareToIgnoreCase("yes")==0){
				myScenario.getParameters().setPartialKills(true);
			}
		} else if (command.compareToIgnoreCase("pit_inner") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangePitInner(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid pit_inner");
			}
		} else if (command.compareToIgnoreCase("pit_outer") == 0){
			String arg = vector.remove(0);
			try{
				double range = Double.parseDouble(arg);
				myScenario.getParameters().setRangePitOuter(range);
			} catch (Exception e){
				Logger.err(this,0, "invalid pit_outer");
			}
		} else if (command.compareToIgnoreCase("radar_time") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg); //TODO parse time
				myScenario.getParameters().setRadarUpdateTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid radar_time");
			}
		} else if (command.compareToIgnoreCase("real_time") == 0){
			if (vector.isEmpty()) return;
			String arg = vector.remove(0);
			if (arg.compareToIgnoreCase("no")==0){
				myScenario.getParameters().setRealTimeSynch(false);
			} else if (arg.compareToIgnoreCase("yes")==0){
				myScenario.getParameters().setRealTimeSynch(true);
				myScenario.getParameters().setRealTimeRatio(1.0);
				gameClock.setSynchPoint();
			} else {
				try{
					double ratio = Double.parseDouble(arg);
					myScenario.getParameters().setRealTimeRatio(ratio);
					myScenario.getParameters().setRealTimeSynch(true);
					gameClock.setSynchPoint();
				} catch (Exception e){
					Logger.err(this,0, "invalid real_time");
				}
			}
		} else if (command.compareToIgnoreCase("run_type") == 0){
			if (vector.isEmpty()) return;
			String arg = vector.remove(0);
			if (arg.compareToIgnoreCase("normal")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_NORMAL);
			} else if (arg.compareToIgnoreCase("checkpoint")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_CHECKPOINT);
			} else if (arg.compareToIgnoreCase("branchpoint")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_BRANCHPOINT);
			} else if (arg.compareToIgnoreCase("branch_plan")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_BRANCH_WITH_PLANNING);
			} else if (arg.compareToIgnoreCase("noninteractive")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_NON_INTERACTIVE);
			} else if (arg.compareToIgnoreCase("batch")==0){
				myScenario.getParameters().setRunType(Constants.RUN_TYPE_NON_INTERACTIVE_BATCH);
			}
		} else if (command.compareToIgnoreCase("start_time") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg); //TODO parse time
				myScenario.getParameters().setStartTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid start_time");
			}
		} else if (command.compareToIgnoreCase("time_step") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg); //TODO parse time
				myScenario.getParameters().setIncrementAmount(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid time_step");
			}
		} else if (command.compareToIgnoreCase("weather_speed") == 0){
			String arg = vector.remove(0);
			try{
				double value = Double.parseDouble(arg);
				myScenario.getParameters().setWeatherMove(value);
			} catch (Exception e){
				Logger.err(this,0, "invalid weather_speed");
			}
		}
	}
	
}
