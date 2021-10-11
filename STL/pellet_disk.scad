module disk(){
difference(){
import("/home/nicoluarte/3dprints/FED3_DISK.stl");

for(a = [0:7])

rotate([0,0,0 + (a*45)])
    color("blue")
    
    translate([15.61,0, 0])
    cylinder (h = 20, r=2.85,
    center = true, $fn=100);
	rotate ([90,0,0]);
difference(){
color("red")
translate([0, 0, -1])
cylinder(h = 12, r = 22);
    translate([0, 0, -1])
cylinder(h = 13, r = 19.5, $fn=100);
}
}
}

module tight_fit(){
mirror([0,1,0])
translate([-2.5,1.35,0])
color("red") cube([5,1,9]);

translate([-2.5,1.35,0])
color("red") cube([5,1,9]);
}

module thicker_disk(){
difference(){
    difference(){
    cylinder(h = 3.5, r = 20, $fn=100);
    translate([0, 0, -10])
    cylinder(h = 15, r = 6);
    }
    for(a = [0:7])

    rotate([0,0,0 + (a*45)])
        color("blue")
        
        translate([15.61,0, 0])
        cylinder (h = 20, r=2.85,
        center = true, $fn=100);
        rotate ([90,0,0]);
}
}


disk();
thicker_disk();
//tight_fit();