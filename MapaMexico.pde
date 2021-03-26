import java.io.InputStreamReader;
import java.util.concurrent.TimeUnit;
import controlP5.*;

PImage mapimg;
ControlP5 cp5;
PrintWriter output;
String text1, text2;

boolean map_on = false;

int clat = 25;
int clon = -100;

int ww = 1024;
int hh = 512;

int zoom = 4;
String[] ciudades;
String[] nombres;
float[] latF, lonF;

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
   background(0);
   
   output = createWriter("input.txt");
   cp5 = new ControlP5(this);
   cp5.addTextfield("Ciudad inicial").setPosition(12, 300).setSize(75, 20).setAutoClear(false);
   cp5.addTextfield("Ciudad final").setPosition(112, 300).setSize(75, 20).setAutoClear(false);
   cp5.addBang("Submit").setPosition(112, 350).setSize(50, 20);
}

void draw () {
  stroke(255); fill(255);
  rect(512-150, 20, 300, 20);
  stroke(0, 0, 0); fill(0, 0, 0);
  if (map_on) {
    for (int i = 0; i < latF.length; i++) {
       if (dist(mouseX-(width/2), mouseY-(height/2), latF[i], lonF[i]) < 3) {
         textSize(12);
         text(nombres[i-2].toUpperCase(), 512-145, 32);
         break;
       }
    }
  }
}

void RunLisp() {
  try {
    Process p = Runtime.getRuntime().exec(new String[] {dataPath("..//astar.exe")});
    try {Thread.sleep(1200);} catch (InterruptedException e) { }
    p.destroy();
    MapMap();
  } 
    catch(Exception e) {
    println(e);
  }
}

void MapMap() {
  map_on = true;
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
  latF = new float[ciudades.length];
  lonF = new float[ciudades.length];
  float prevx=0;
  float prevy=0;

  for (int i = 2; i < ciudades.length; i++) {
    String[] datos = ciudades[i].split(",");

    lat[i] = float(datos[0]);
    lon[i] = -float(datos[1]);
   
    float x = mercX(lon[i]) - cx;
    float y = mercY(lat[i]) - cy;


    if(x < - width/2) {
      x += width;
    } else if(x > width / 2) {
      x -= width;
    }
    if(i>2){
      prevx= mercX(lon[i-1]) - cx;
      prevy= mercY(lat[i-1]) - cy;
      line(x,y,prevx,prevy);
    }
    stroke(255, 0, 255);
    fill(255, 0, 255, 200);
    ellipse(x, y, 1, 1);
    latF[i] = x;
    lonF[i] = y;
  }
  textSize(12);
  stroke(255);
  fill(255);
  text("la distancia es " + ciudades[0] + " km.",-500,150);
  nombres = ciudades[1].split(" - ");
  output = createWriter("input.txt");
}

void Submit() {
  println();
  print("this is the text you typed :");
  text1 = cp5.get(Textfield.class, "Ciudad inicial").getText();
  cp5.get(Textfield.class, "Ciudad inicial").clear();
  text2 = cp5.get(Textfield.class, "Ciudad final").getText();
  cp5.get(Textfield.class, "Ciudad final").clear();
  println(text1);
  print("this is the text you typed :");
  println(text2);
  output.println(text1);
  output.println(text2);
  output.flush();
  output.close();
  RunLisp();
}
