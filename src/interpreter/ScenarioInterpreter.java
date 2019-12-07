package interpreter;

import java.util.Vector;

import sim.Constants;
import sim.GameClock;
import sim.Scenario;
import utils.Logger;
import utils.Parser;
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
		} else if (command.compareToIgnoreCase("auto_save") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setCheckPointFrequency(clock);
			} else {
				Logger.err(this,0, "invalid auto_save time");
			}
		} else if (command.compareToIgnoreCase("casualty_cycle") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setCasualtyCycleTime(clock);
			} else {
				Logger.err(this,0, "invalid casualty_cycle");
			}
		} else if (command.compareToIgnoreCase("casualty_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setCasualtyModel(modelName);
		} else if (command.compareToIgnoreCase("defilade_time") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setDefiladeTime(clock);
			} else {
				Logger.err(this,0, "invalid defilade time");
			}
		} else if (command.compareToIgnoreCase("detect_obstacle_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setDetectObstacleModel(modelName);
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
		} else if (command.compareToIgnoreCase("detect_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setDetectModel(modelName);
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
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(gameClock.getClockSecs(), arg);
			if ( clock >=0 ){
				myScenario.getParameters().setEndTime(clock);
			} else {
				Logger.err(this,0, "invalid end_time");
			}
		} else if (command.compareToIgnoreCase("file") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		} else if (command.compareToIgnoreCase("fire_cycle") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setDirectFireCycleTime(clock);
			} else {
				Logger.err(this,0, "invalid fire_cycle time");
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
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setMovementCycleTime(clock);
			} else {
				Logger.err(this,0, "invalid move_cycle time");
			}
		} else if (command.compareToIgnoreCase("movement_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setMovementModel(modelName);
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
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setRadarUpdateTime(clock);
			} else {
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
		} else if (command.compareToIgnoreCase("resupply_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setResupplyModel(modelName);
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
		} else if (command.compareToIgnoreCase("scan_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setScanModel(modelName);
		} else if (command.compareToIgnoreCase("shoot_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setShootModel(modelName);
		} else if (command.compareToIgnoreCase("start") == 0){
			//TODO should implement yes/ no [default yes] and set param in sim accordingly
		} else if (command.compareToIgnoreCase("start_time") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(gameClock.getClockSecs(), arg);
			if ( clock >=0 ){
				myScenario.getParameters().setStartTime(clock);
			} else {
				Logger.err(this,0, "invalid start_time");
			}
		} else if (command.compareToIgnoreCase("time_step") == 0){
			double clock = -1.0;
			String arg = vector.remove(0);
			clock = Parser.parseFormattedTime(arg);
			if ( clock >=0 ){
				myScenario.getParameters().setClockUpdateTime(clock);
			} else {
				Logger.err(this,0, "invalid time_step");
			}
		} else if (command.compareToIgnoreCase("suppression_model") == 0){
			if (vector.isEmpty()) return;
			String modelName = vector.remove(0);
			myScenario.setSuppressionModel(modelName);
		} else if (command.compareToIgnoreCase("weather_speed") == 0){
			String arg = vector.remove(0);
			try{
				double value = Double.parseDouble(arg);
				myScenario.getParameters().setWeatherMove(value);
			} catch (Exception e){
				Logger.err(this,0, "invalid weather_speed");
			}
		} else {
			Logger.err(this, Logger.WARNING, "unknown command " + command + " " + vector);
		}
	}
	
}
