package data.map;

public class MapTransposer {
	
	private double viewOffsetX;
	private double viewOffsetY;
	private double screenOffsetX;
	private double screenOffsetY;
	private double ratioX;
	private double ratioY;
	
	public MapTransposer(Map myMap, int panelWidth, int panelHeight){
//		int panelWidth = this.getWidth()-3;
//		int panelHeight = this.getHeight()-4;
		int screenWidth = panelWidth; //TODO adjust to centre image
		int screenHeight = panelHeight;
		double mapWidth = myMap.getSizeX();
		double mapHeight = myMap.getSizeY();
		double viewWidth = mapWidth;// TODO should account for zooming
		double viewHeight = mapHeight;
		double mapXll = myMap.getLL().getX();
		double mapYll = myMap.getLL().getY();
		double mapXur = myMap.getUR().getX();
		double mapYur = myMap.getUR().getY();
		double viewXll = mapXll;
		double viewYll = mapYll;
		double viewXur = mapXur;
		double viewYur = mapYur;
		
		screenOffsetX = 1;
		screenOffsetY = screenHeight;
		
		if (screenWidth < screenHeight){
			//
		} else {
			//
		}
		double screenXll = 1;
		double screenYll = screenHeight;
		double screenXur = screenWidth;
		double screenYur = 1;
		
		viewOffsetX = viewXll;
		viewOffsetY = viewYll;
		ratioX = screenWidth/ viewWidth;
//		System.out.println("ratio " + ratioX + " : " + viewWidth + " : " + screenWidth);
		ratioY = screenHeight/ viewHeight;
		System.out.println("ratio " + ratioY + " : " + viewHeight + " : " + screenHeight);
		
	}

	public int map2screenX(double x){
		double mapx = x;
		mapx = mapx - viewOffsetX;
		double screenX = mapx * ratioX;
//		System.out.println("calc " + viewOffsetX + " : " + mapx + " : " + screenX);
		screenX = screenX + screenOffsetX;
		int ix = (int) screenX;
		return ix;
	}
	
	public int map2screenY(double y){
		double mapy = y;
		mapy = mapy - viewOffsetY;
		double screenY = mapy * ratioY;
		screenY = screenOffsetY - screenY;
		System.out.println("calc " + viewOffsetY + " : " + mapy + " : " + screenY);
		int iy = (int) screenY;
		return iy;
	}
	public int screen2MapX(int x){return 0;}
	public int screen2MapY(int x){return 0;}
}
