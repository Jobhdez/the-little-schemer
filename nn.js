const V = require('./vector')
const M = require('./matrix.js')

class NeuralNet{
    constructor(vec) {
	this.vec = vec
    }
    softmax() {
	let expV = this.vec.map((element) => {
	    return Math.exp(element)
	})
	
	const initialValue = 0
	
	let sumExps = expV.reduce((accumulator, currentValue) => accumulator + currentValue + initialValue)

	let sVec = expV.map((element) => { return element / sumExps})

	return new V.Vector(sVec)
    }

    softmax2d() {
	return new M.Matrix(this.vec.map((v) => {
	    let smax = new NeuralNet(v).softmax()
	    return smax.vector
	}))
    }
			
	    
    logSoftmax() {
	let softVec = this.softmax(this.vec)

	return new V.Vector(softVec.vector.map((element) => { return Math.log(element) }))
    }

    logSoftmax2d() {
	return new M.Matrix(this.vec.map((v) => {
	    let lsmax = new NeuralNet(v).logSoftmax()
	    return lsmax.vector}))
    }

    relu() {
	return new V.Vector(this.vec.map((v) => { return Math.max(0, v) }))
    }

    relu2d() {
	return new M.Matrix(this.vec.map((row) => { return row.map((column) => { return Math.max(0, column) })}))
    }

    sigmoid() {
	return new V.Vector(this.vec.map((n) => { return 1 / (1 + Math.exp(- n))}))
    }

    sigmoid2d() {
	return new M.Matrix(this.vec.map((row) => { return row.map((n) => { return 1 / (1 + Math.exp(- n))})}))
    }

    tanh() {
	return new V.Vector(this.vec.map((n) => { return (Math.exp(n) -  Math.exp(- n)) / (Math.exp(n) + Math.exp(- n))}))
    }

    tanh2d() {
	return new M.Matrix(this.vec.map((row) => { return row.map((n) => {return (Math.exp(n) -  Math.exp(- n)) / (Math.exp(n) + Math.exp(- n))})}))
    }
    
    convolution(other) {
	let t = (this.vec.length + other.length) - 1
	var convolutions = []
	for (let i = 0; i < t; i++) {
	    convolutions.push(null)
	}
	for (let i in convolutions) {
	    var summation = 0
	    for (let j in this.vec) {
		if (i - j >= 0 && i - j < this.vec.length) {
		    summation += this.vec[j] * other[i-j]
		}
	    }
	    convolutions[i] = summation
	}

	return new V.Vector(convolutions)
    }
		
	
}

module.exports = {NeuralNet}
