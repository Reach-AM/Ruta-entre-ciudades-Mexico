// Daniel Shiffman
// http://codingtra.in
// Earthquake Data Viz
// Video: https://youtu.be/ZiYdOwOrGyc

PImage mapimg;

int clat = 25;
int clon = -100;

int ww = 1024;
int hh = 512;

int zoom = 4;
String[] ciudades;


float mercX(float lon) {
  lon = radians(lon);
  float a = (256 / PI) * pow(2, zoom);
  float b = lon + PI;
  return a * b;
}

float mercY(float lat) {
  lat = radians(lat);
  float a = (256 / PI) * pow(2, zoom);
  float b = tan(PI / 4 + lat / 2);
  float c = PI - log(b);
  return a * c;
}


void setup() {
  size(1024, 512);
  // The clon and clat in this url are edited to be in the correct order.
  String url = "https://api.mapbox.com/styles/v1/mapbox/dark-v9/static/" +
    clon + "," + clat + "," + zoom + "/" +
    ww + "x" + hh +
    "?access_token=pk.eyJ1IjoiZWFnbGV3aXRoZ2xhc3NlcyIsImEiOiJja21iZ2licmwxb3d0MndzOXJwcXRrODd4In0.qOhc-GeTEiJ5E2T9Nvujng";
  mapimg = loadImage(url, "jpg");
 ciudades= loadStrings("output.txt");



  translate(width / 2, height / 2);
  imageMode(CENTER);
  image(mapimg, 0, 0);

  float cx = mercX(clon);
  float cy = mercY(clat);
  float lat[] = new float[ciudades.length];
  float lon[] = new float[ciudades.length];
  float prevx=0;
  float prevy=0;

print(ciudades.length+"length, I:");
  for (int i = 1; i < ciudades.length; i++) {
    String[] datos = ciudades[i].split(",");
    print(i+" ");
    lat[i] = float(datos[0]);
    lon[i] = float(datos[1]);
   
    float x = mercX(lon[i]) - cx;
    float y = mercY(lat[i]) - cy;
    // This addition fixes the case where the longitude is non-zero and
    // points can go off the screen.
    if(x < - width/2) {
      x += width;
    } else if(x > width / 2) {
      x -= width;
    }
    if(i>1){
      
      prevx= mercX(lon[i-1]) - cx;
      prevy= mercY(lat[i-1]) - cy;
      line(x,y,prevx,prevy);
    }
    stroke(255, 0, 255);
    fill(255, 0, 255, 200);
    ellipse(x, y, 1, 1);
    
  }
  text(ciudades[0],-500,-200);
}
