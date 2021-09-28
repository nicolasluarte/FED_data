
module wall_side (x,y,z, tol) {
// variables
ancho_placa = 43.2;  // this controls how tight is the fit
grosor_placa = 3;   
grosor_soporte = 3;
grosor_extra = 4;  

cube([x,y,z]);

//// plataforma para asegurar que no haya tope en el la pieza con los bordes de la jaula. Hay que considerar el anchon de los soportes de la jaula
hull() {
    translate([3,0,15]) color("red") cube([x-6,y,3]); // crea soporte usando funcion hull()
    translate([3,0,12]) cube([x-6,8,3]); 
    }
//// plataforma que va a sujetar el panel de acero
translate([0,5,12]) 
    difference() {
        translate([-grosor_extra/2,1,0])
        cube([x+grosor_extra,x,10]);
        translate([(x - ancho_placa)/2,3,3]) 
        cube([ancho_placa,x,grosor_placa]); // substraccion 1
        translate([(x - (ancho_placa-grosor_soporte*2))/2,3,3])
        cube([ancho_placa-grosor_soporte*2,x+tol,10]); // substraccion 1     
    } 
}

module cable_hole(length, width){
        ancho_placa = 43;  
           rotate([90, 0, 0]){
            translate([ancho_placa/2, ancho_placa/3.2, -18])
            color("red") cylinder(length, width, width, $fn = 100);
        }     
}

module cut(){
translate([-10,-10,0])
cube([60,40,40]);
}

    
    
 /* 
CREATE OBJECTS 
DIMENSIONS
wall_side
x = 43.65 - 43.2 (print) wide
y = 2.22 - 2.15 (print) depth
z = 88 (print) height

spout_holder
x = 25
y = 35;
z = 35;

other dimensions
11 mm space between bottom wall_side and spout_holder
*/

tol = 0.5;
w = 2.5; // thickness of wall
difference(){
wall_side(43.2, 2.15,88, tol);
cable_hole(100, 3.5);
}
