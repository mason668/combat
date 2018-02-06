package data.map;

public class MapTransposer {
	
	private double viewOffsetX;
	private double viewOffsetY;
	private double screenOffsetX;
	private double screenOffsetY;
	private double ratioX;
	private double ratioY;
	
	public MapTransposer(){
	}
	
	public void update(Map myMap, int panelWidth, int panelHeight) { 
		int imageWidth = panelWidth;
		int imageHeight = panelHeight;
		if ( imageWidth > imageHeight ) imageWidth = imageHeight;
		if ( imageHeight > imageWidth ) imageHeight = imageWidth;

		double mapWidth = myMap.getWidth();
		double mapHeight = myMap.getHeight();
		double viewWidth = mapWidth;// TODO should account for zooming
		double viewHeight = mapHeight;
		double mapXll = myMap.getLL().getX();
		double mapYll = myMap.getLL().getY();
		double viewXll = mapXll;
		double viewYll = mapYll;
		
		screenOffsetX = (panelWidth - imageWidth)/2;
		screenOffsetY = panelHeight - (panelHeight - imageHeight)/2;
		
		viewOffsetX = viewXll;
		viewOffsetY = viewYll;
		ratioX = imageWidth/ viewWidth;
		ratioY = imageHeight/ viewHeight;
	}

	public int map2screenX(double x){
		double mapx = x;
		mapx = mapx - viewOffsetX;
		double screenX = mapx * ratioX;
		screenX = screenX + screenOffsetX;
		int ix = (int) screenX;
		return ix;
	}
	
	public int map2screenY(double y){
		double mapy = y;
		mapy = mapy - viewOffsetY;
		double screenY = mapy * ratioY;
		screenY = screenOffsetY - screenY;
		int iy = (int) screenY;
		return iy;
	}
	public double screen2MapX(int x){
		double screenX = x;
		screenX = screenX - screenOffsetX;
		double mapX = screenX / ratioX;
		mapX = mapX + viewOffsetX;
		return mapX;
	}
	public double screen2MapY(int y){
		double screenY = y;
		screenY = screenY - screenOffsetY;
		double mapY = screenY / ratioY;
		mapY = viewOffsetY - mapY;
		return mapY;
	}
	
	public Coordinate screen2Map(int x, int y){
		return new Coordinate(screen2MapX(x),screen2MapY(y));
	}
}
