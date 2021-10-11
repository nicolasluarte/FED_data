import("/home/nicoluarte/3dprints/FED_SILO.stl");
module sep(){
difference(){
translate([-20, 0, 20])
color("blue") rotate([0, 80, 0])
cylinder(h = 20, r1 = 15, r2 = 1, $fn = 100);

translate([-24, -20, 24.3])
color("red") 
cube([40, 40, 60]);
    
translate([-15, 0, 1.6])
color("pink")
cube([10, 20, 10], center = true);
}
}

module ring(){
translate([0, 0, 15])
color("pink")
difference(){
cylinder(10, r = 25.3, $fn = 100);
translate([0, 0, -1])
cylinder(12, r = 23.21, $fn = 100);
}
}

difference(){
sep();
ring();
}
translate([-37.5, -20, 6.54])
color("red") cube([20, 40, 16]);

