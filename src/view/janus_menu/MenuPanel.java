package view.janus_menu;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

public class MenuPanel extends JPanel{
	private static final long serialVersionUID = 1L;
	
	private ModeMenu modeMenu = new ModeMenu();
	private CounterMenu counterMenu = new CounterMenu();
	private OrderMenu orderMenu = new OrderMenu();
	private ScopeMenu scopeMenu = new ScopeMenu();
	private ReportMenu reportMenu = new ReportMenu();
	private SpecialistMenu specialistMenu = new SpecialistMenu();
	private MapMenu mapMenu = new MapMenu();
	
	public MenuPanel(){
		super();
		this.setBackground(Color.GRAY);
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		this.add(modeMenu);
		this.add(counterMenu);
		this.add(orderMenu);
		this.add(scopeMenu);
		this.add(reportMenu);
		this.add(specialistMenu);
		this.add(mapMenu);
	}

}
