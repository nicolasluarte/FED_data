
module sensor_slot (x,y,z, tol) {
// variables
<<<<<<< HEAD
ancho_placa = 43.1;  
=======
ancho_placa = 43.2;  // this controls how tight is the fit
>>>>>>> 6038d3d166c52dbd35ba64724e83e222b6e75e6d
grosor_placa = 3;   
grosor_soporte = 3;
grosor_extra = 4;  


//// plataforma que va a sujetar el panel de acero
translate([0,5,12]) 
    difference() {
        translate([-grosor_extra/2,1,0])
        cube([x+grosor_extra,x+4,10]);
        translate([(x - ancho_placa)/2,3,3]) 
        cube([ancho_placa,x+4,grosor_placa]); // substraccion 1
        translate([(x - (ancho_placa-grosor_soporte*2))/2,3,3])
        cube([ancho_placa-grosor_soporte*2,x+tol+4,10]); // substraccion 1     
    }
   // this is the cover
   translate([0, 6, 20])
   color("red") cube([x, 20, 2]); 
}

module wall_mount(x,y,z){


cube([x,y,z]);
reinforcement(x,y);

}

module reinforcement(x,y){
    hull() {
    translate([3,0,15]) color("red") cube([x-6,y,3]); // crea soporte usando funcion hull()
    translate([3,0,12]) cube([x-6,8,3]); 
    }
}


module cable_hole(length, width){
        ancho_placa = 43;  
           rotate([90, 0, 0]){
            translate([ancho_placa/2, ancho_placa/2.5, -10])
            color("red") cylinder(length, width, width, $fn = 100);
        }     
}

module cut(){
translate([-10,-10,0])
cube([60,40,40]);
}

    
tol = 0.5;
w = 2.5; // thickness of wall

difference(){
    wall_mount(43.2, 2.15,88);
    cable_hole(100, 2);
}
difference(){

sensor_slot(43.2, 2.15,88, tol);
cable_hole(100, 2);
    
}
