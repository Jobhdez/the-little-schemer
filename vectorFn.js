let poly = require('./poly');

class VectorFunction {
    constructor(vectorFn) {
	this.vectorFn = vectorFn
    }
    diff() {
	/*
	  `diff` takes the derivative of a given vector valued function where the vector consists of polynomials."

	  @param this.vector
	  @returns: derivative of given vector 
	*/
	return new VectorFunction(this.vectorFn.map((n) => { return new poly.Polynomial(n).diff() }))
	    
    }
}

module.exports = { VectorFunction }
