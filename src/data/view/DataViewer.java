package data.view;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import data.CSData;
import data.csd.PHTable;
import data.csd.Weapon;
import data.managers.PHTableList;
import data.managers.PlatformList;
import data.managers.WeaponList;
import data.view.platforms.MenuPanelPlatformCharacteristics;
import data.view.platforms.MenuPanelPlatformCharacteristics1;
import data.view.platforms.MenuPanelPlatformCharacteristics2;
import data.view.platforms.MenuPanelPlatformCharacteristics3;
import data.view.platforms.MenuPanelPlatformDetectionData;
import data.view.platforms.MenuPanelPlatformDetectionHeights;
import data.view.platforms.MenuPanelPlatforms;
import models.directfire.view.MenuPanelModelDirectFire;
import models.directfire.view.MenuPanelModelDirectFirePH;
import models.directfire.view.MenuPanelModels;

public class DataViewer extends JFrame implements ActionListener{
	
	private static final long serialVersionUID = 1L;
	private CSData csdata;
	private Vector<String> panelList = new Vector<String>();

	public static void main(String args[]){
		DataViewer me = new DataViewer();
		me.test();
	}
	
	private void test(){
		csdata = new CSData();
		csdata.maketestData();
	}
	
	private MenuPanel myMenuPanel;
	private JPanel panel;
	private JScrollPane pane;
	
	public DataViewer(){
		super("CSData");
		registerEventHandlers();
		this.setSize(800, 600);
		this.setVisible(true);
		makeGUI();
		this.validate();
	}
	
	private void registerEventHandlers(){
		this.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);;
					}
				}
		);
	}
	
	private void makeGUI(){
		panel = new JPanel();
		pane = new JScrollPane(panel);
		this.add(pane);
		panel.setLayout(new FlowLayout());
		myMenuPanel = new MenuPanelMain(this);
		panel.add(myMenuPanel);
		panel.validate();
	}
	
	@Override
	public void actionPerformed(ActionEvent event) {
		String action = event.getActionCommand();
		this.changePanel(action, "");
	}
	
	public void changePanel(String action, String actionOption){
		System.out.println("hit " + action + ":" + actionOption);
		panel.remove(myMenuPanel);
		if (action.compareToIgnoreCase("back")==0){
			if (panelList.size()<2){
				myMenuPanel = new MenuPanelMain(this);
				panel.add(myMenuPanel);
				panel.revalidate();
				panel.repaint();
				return;
			} else {
				action = panelList.remove(panelList.size()-1);
				action = panelList.remove(panelList.size()-1);
			}
		}
		panelList.addElement(action);
		if ( action.compareToIgnoreCase("")==0 ){
		} else if (action.compareToIgnoreCase("chemical & heat data")==0){
			myMenuPanel = new MenuPanelChemical(this);
		} else if (action.compareToIgnoreCase("chemical susceptibility data")==0){
			myMenuPanel = new MenuFormChemicalSusceptibility(this, csdata);
		} else if (action.compareToIgnoreCase("data set by weapon")==0){
			WeaponList list = csdata.getWeaponList();
			myMenuPanel = new MenuPanelSelectWeapon(this, list, "data set by weapon actual");
		} else if (action.compareToIgnoreCase("data set by weapon actual")==0){
			Weapon weapon = csdata.getWeaponList().getWeapon(actionOption);
			PlatformList list = csdata.getPlatformList(); //TODO validate
			myMenuPanel = new MenuPanelWeaponDataSets(this, weapon, list);

		} else if (action.compareToIgnoreCase("detection data")==0){
			PlatformList list = csdata.getPlatformList();
			if ( list != null){
				myMenuPanel = new MenuPanelPlatformDetectionData(this, list);
			}
		} else if (action.compareToIgnoreCase("detection heights")==0){
			PlatformList list = csdata.getPlatformList();
			if ( list != null){
				myMenuPanel = new MenuPanelPlatformDetectionHeights(this, list);
			}
		} else if (action.compareToIgnoreCase("direct fire model")==0){
			myMenuPanel = new MenuPanelModelDirectFire(this);
		} else if (action.compareToIgnoreCase("models")==0){
			myMenuPanel = new MenuPanelModels(this);
		} else if (action.compareToIgnoreCase("ph data sets")==0){
			PHTableList list = csdata.getPHTableList();
			myMenuPanel = new MenuPanelPHDatasets(this, list);
		} else if (action.compareToIgnoreCase("ph table")==0){
			PHTable myPHTable = csdata.getPHTableList().get(actionOption);
			if (myPHTable != null){
			myMenuPanel = new MenuPanelPHTable(this, myPHTable);
			}
		} else if (action.compareToIgnoreCase("platforms")==0){
			myMenuPanel = new MenuPanelPlatforms(this);
		} else if (action.compareToIgnoreCase("platform characteristics")==0){
			myMenuPanel = new MenuPanelPlatformCharacteristics(this);
		} else if (action.compareToIgnoreCase("platform characteristics 1")==0){
			PlatformList list = csdata.getPlatformList();
			if ( list != null){
				myMenuPanel = new MenuPanelPlatformCharacteristics1(this, list);
			}
		} else if (action.compareToIgnoreCase("platform characteristics 2")==0){
			PlatformList list = csdata.getPlatformList();
			if ( list != null){
				myMenuPanel = new MenuPanelPlatformCharacteristics2(this, list);
			}
		} else if (action.compareToIgnoreCase("platform characteristics 3")==0){
			PlatformList list = csdata.getPlatformList();
			if ( list != null){
				myMenuPanel = new MenuPanelPlatformCharacteristics3(this, list);
			}
		} else if (action.compareToIgnoreCase("test ph")==0){
			myMenuPanel = new MenuPanelModelDirectFirePH(this, csdata);
		} else if (action.compareToIgnoreCase("weapons")==0){
			myMenuPanel = new MenuPanelWeapons(this);
		} else if (action.compareToIgnoreCase("exit")==0){
			System.exit(0);
		} else {
			JOptionPane.showMessageDialog(this, action);
			panelList.remove(panelList.size()-1);
		}
		panel.add(myMenuPanel);
		panel.revalidate();
		panel.repaint();		
	}
	
}
