package view.menu;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

public class MenuPanel extends JPanel{
	
	private DeployButton deployButton = new DeployButton();
	private MoveButton moveButton = new MoveButton();
	private MenuButton showButton = new MenuButton("Show");
	private MenuButton stopButton = new MenuButton("Stop");
	private MenuButton goButton = new MenuButton("Go");
	private MenuButton speedButton = new MenuButton("Speed");
	private MenuButton mountButton = new MenuButton("Mount");
	private MenuButton transferButton = new MenuButton("Transfer");
	private MenuButton assignButton = new MenuButton("Assign");
	private MenuButton dismountButton = new MenuButton("Dismount");
	private MenuButton defiladeButton = new MenuButton("Defl");
	private MenuButton pitButton = new MenuButton("Pit");
	private MenuButton viewButton = new MenuButton("View");
	private MenuButton losButton = new MenuButton("LOS");
	private FaceButton faceButton = new FaceButton();

	private MenuButton addButton = new MenuButton("Add");
	private MenuButton alterButton = new MenuButton("Alter");
	private MenuButton deleteButton = new MenuButton("Delete");
	private MenuButton cancelButton = new MenuButton("Cancel");
	private MenuButton copyButton = new MenuButton("Copy");
	
	public MenuPanel(){
		super();
		setBackground(Color.WHITE);
		makePanels();
	}
	
	private void makePanels(){
		//TODO change to menubuttons
		
		this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
		this.add(makeOrdersPanel());
	}
	
	private JPanel makeOrdersPanel(){
		JPanel panel1 = new JPanel();
		panel1.setBackground(Color.WHITE);
		panel1.setLayout(new BorderLayout());
		
		//JPanel line3 = new JPanel();
		JTabbedPane tabbedPane = new JTabbedPane();
		tabbedPane.setBackground(Color.WHITE);
		
		tabbedPane.addTab("Manouevre",makeMovePanel());
		tabbedPane.addTab("Route", makeRoutePanel());
		tabbedPane.addTab("DFir", makeDfirPanel());
		tabbedPane.addTab("IFir", makeIfirPanel());
		tabbedPane.addTab("ISR", makeIsrPanel());
		tabbedPane.addTab("Form", makeFormPanel());
		tabbedPane.addTab("Avn", makeAvnPanel());
		tabbedPane.addTab("Eng", makeEngPanel());
		tabbedPane.addTab("Log", makeLogPanel());
		tabbedPane.addTab("Urban", makeUrbanPanel());
		tabbedPane.addTab("SOP", makeSopPanel());
		tabbedPane.addTab("Misc", makeMiscPanel());
		
		panel1.add(tabbedPane, BorderLayout.CENTER);
		return panel1;
	}
	
	private JPanel makeMovePanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		
		JPanel line1 = new JPanel();
		line1.setLayout(new BoxLayout(line1,BoxLayout.LINE_AXIS));
		line1.add(deployButton);
		line1.add(moveButton);
		line1.add(showButton);
		line1.add(speedButton);
		panel.add(line1);
		
		JPanel line2 = new JPanel();
		line2.setLayout(new BoxLayout(line2,BoxLayout.LINE_AXIS));
		line2.add(stopButton);
		line2.add(goButton);
		panel.add(line2);

		JPanel line3 = new JPanel();
		line3.setLayout(new BoxLayout(line3,BoxLayout.LINE_AXIS));
		line3.add(mountButton);
		line3.add(dismountButton);
		line3.add(transferButton);
		line3.add(assignButton);
		line3.add(pitButton);
		panel.add(line3);

		JPanel line4 = new JPanel();
		line4.setLayout(new BoxLayout(line4,BoxLayout.LINE_AXIS));
		line4.add(viewButton);
		line4.add(losButton);
		line4.add(faceButton);
		line4.add(defiladeButton);
		panel.add(line4);

		// route alt del can copy

		return panel;
	}

	private JPanel makeRoutePanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));

		JPanel line1 = new JPanel();
		line1.setLayout(new BoxLayout(line1,BoxLayout.LINE_AXIS));
		line1.add(addButton);
		line1.add(alterButton);
		line1.add(deleteButton);
		line1.add(cancelButton);
		line1.add(copyButton);
		panel.add(line1);

		// route alt del can copy
		return panel;
	}
	private JPanel makeDfirPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeIfirPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeIsrPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeFormPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeAvnPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeEngPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeLogPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeUrbanPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeSopPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}
	private JPanel makeMiscPanel(){
		JPanel panel = new JPanel();
		panel.setBackground(Color.WHITE);
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		return panel;
	}

	public void setMenuGroup(MenuGroup menuGroup){
		if (menuGroup == null) return;
		menuGroup.addMenuButton(deployButton);
		menuGroup.addMenuButton(moveButton);
		menuGroup.addMenuButton(showButton);
		menuGroup.addMenuButton(stopButton);
		menuGroup.addMenuButton(goButton);
		menuGroup.addMenuButton(speedButton);
		menuGroup.addMenuButton(mountButton);
		menuGroup.addMenuButton(transferButton);
		menuGroup.addMenuButton(assignButton);
		menuGroup.addMenuButton(dismountButton);
		menuGroup.addMenuButton(defiladeButton);
		menuGroup.addMenuButton(pitButton);
		menuGroup.addMenuButton(viewButton);
		menuGroup.addMenuButton(losButton);
		menuGroup.addMenuButton(faceButton);

		menuGroup.addMenuButton(addButton);
		menuGroup.addMenuButton(alterButton);
		menuGroup.addMenuButton(deleteButton);
		menuGroup.addMenuButton(cancelButton);
		menuGroup.addMenuButton(copyButton);

	}
}
