class MatrixVector {
    constructor(matrix, vector) {
	this.matrix = matrix;
	this.vector = vector;
    }
    matrixVecMul() {
	/*
	  @param: matrix
	  @param: vector
	  @returns: the product of the matrix and vector 
	 */
	let interimMatrix = [];

	let rows = this.matrix.length;
	let columns = this.matrix[0].length;

	// initialize matrix 
	for (let i = 0; i < rows; i++) {
	    interimMatrix[i] = [];
	}

	for (let i = 0; i < rows; i++) {
	    for (let j = 0; j < columns; j++) {
		for (let k = 0; k < this.vector.length; k++) {
		    interimMatrix[i][j] = this.matrix[i][j] * this.vector[k];
		}
	    }
	}
	let resultVector = [];
	let sum;
	for (let i = 0; i < interimMatrix.length; i++) {
	    sum = 0;
	    for (let j = 0; j < interimMatrix[i].length; j++) {
		sum += interimMatrix[i][j];
	    }
	    resultVector[i] = sum;
	}

	return resultVector;
    }

    addVectorMat() {
	return this.matrix.map((i) => { return vectorAddition(i, this.vector) })
    }

    subVectorMat() {
	return this.matrix.map((i) => { return vectorSubtraction(i, this.vector)})
    }
}

function vectorAddition(v1, v2) {
    return v1.map((n, i) => { return n + v2[i]})
}

function vectorSubtraction(v1,v2) {
    return v1.map((n, i) => { return n - v2[i]})
}

module.exports = { MatrixVector }
	
