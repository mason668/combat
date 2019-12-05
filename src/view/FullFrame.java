package view;

import java.awt.Dimension;
import java.awt.Toolkit;

import javax.swing.JFrame;

/**
 * This class does the basic work of creating a Frame to contain GUI components.
 *
 */
public class FullFrame extends JFrame {
	private static final long serialVersionUID = 1L;

	public FullFrame(String caption){
		super(caption);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		int height = screenSize.height;
		int width = screenSize.width;
		//this.setPreferredSize(new Dimension(width, height)); //FIXME reset full frame to full screen
		this.setPreferredSize(new Dimension(500, 300));
		this.setMinimumSize(new Dimension(100,100));
	}

}
