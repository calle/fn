//- A simple 2D vector library
//- All angles are in degrees unless otherwise stated

//Vector = {}
//Vector.mt = { __index = Vector }

var Vector = module.exports = function( x, y ){
  // Used as factory
  if (!(this instanceof Vector)) return new Vector(x, y);

  // Allow for invocation with { x:x, y:y } object
  if (typeof x === 'object') {
    y = x.y;
    x = x.x;
  }

	this.x = x
	this.y = y
}

//Vector.prototype.polar = function( r, angle ){
	//var phi = Math.rad( angle )
	//var x = r * Math.cos( phi )
	//var y = r * Math.sin( phi )
	//return new Vector( x, y )
//}

Vector.prototype.add = function( v ){
	return new Vector ( this.x + v.x, this.y + v.y )
}

Vector.prototype.sub = function( v ){
	return new Vector ( this.x - v.x, this.y - v.y )
}

Vector.prototype.scalarMulti = function( s ){
	return new Vector ( s * this.x, s * this.y )
}

Vector.prototype.neg = function( ){
	return this.scalarMulti( -1 )
}

Vector.prototype.equals = function( v ){
	return (this.x == v.x) && (this.y == v.y)
}

Vector.prototype.dotProd = function( v ){
	return this.x * v.x + this.y * v.y
}

Vector.prototype.norm = function( ){
	return Math.sqrt( this.dotProd( this ) )
}

Vector.prototype.unit = function( ){
	return this.scalarMulti ( 1.0 / this.norm() )
}

Vector.prototype.dist = function( v2 ){
	return this.sub( v2 ).norm( )
}

// the scalar projection of the vector on v
Vector.prototype.scalarProj = function( v ){
	var e_v = v.unit()
	return e_v.scalarMulti( this.dotProd( e_v ) )
//	( this.dotProd(v) ):scalarMulti( 1.0 / v:norm() )
}

// the orthogonal projection of the vector on v
Vector.prototype.orthogonalProj = function( v ){
	return this.sub( this.scalarProj( v ) )
}

// rotate degrees
Vector.prototype.rotate = function( deg ){
	var rad = Math.PI * deg / 180;
	return this.rotateRad(rad);
}

Vector.prototype.rotateRad = function( rad ){
  var newx = this.x * Math.cos( rad ) - this.y * Math.sin( rad )
	var newy = this.x * Math.sin( rad ) + this.y * Math.cos( rad )
	return new Vector( newx, newy )
}

// the angel in degrees
Vector.prototype.angle = function( ){
	var rad = this.angleRad();
//	var deg = Math.deg( rad )
	var deg = rad * 180 / Math.PI

	if (this.x < 0){
		deg = deg + 180
	}

	if (deg < 0){
		deg = deg + 360
	}
	return deg
}

// The angel in rad
Vector.prototype.angleRad = function( ){
  // if (this.x === 0) {
  //   console.log('Vector.angleRad: special case for x === 0');
  //   return this.y > 0 ? Math.PI / 2 : 0;
  // }
	return Math.atan( this.y / this.x );
}

Vector.prototype.toString = function() {
	return "(" + this.x + "," + this.y + ")";
}

////////////////////-
// Overload operators

//Vector.mt.__tostring = function ( v )
	//return "(" .. v.x .. "," .. v.y .. ")"
//end

//Vector.mt.__eq = function ( v1, v2 )
	//return v1 : equals ( v2 )
//end

//Vector.mt.__add = function ( v1, v2 )
	//return v1 : add( v2 )
//end

//Vector.mt.__sub = function ( v1, v2 )
	//return v1 : sub( v2 )
//end

//Vector.mt.__unm = function ( v )
	//return v : neg()
//end

//Vector.mt.__mul = function ( v1, v2 )
	//if type(v1) == "table" and type(v2) == "table" then
		//// perform dot product
		//return v1 : dotProd ( v2 )
	//elseif type(v1) == "table" and type(v2) == "number" then
		//return v1 : scalarMulti( v2 )
	//elseif type(v1) == "number" and type(v2) == "table" then
		//return v2 : scalarMulti( v1 )
	//else
		//error ( "unexpected types for vector multiplication" )
	//end
//end
