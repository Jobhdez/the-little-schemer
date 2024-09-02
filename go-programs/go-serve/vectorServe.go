package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"
)

type Vectors struct {
	Vector1 []float64 `json:"vector1"`
	Vector2 []float64 `json:"vector2"`
}

type Matrices struct {
	Matrix1 [][]float64 `json:"matrix1"`
	Matrix2 [][]float64 `json:"matrix2"`
}

type Result struct {
	Exp []float64 `json:"exp"`
}

type MatrixResult struct {
	MExp [][]float64 `json:"exp"`
}
var PORT = ":1234"

func defaultHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host)
	w.WriteHeader(http.StatusNotFound)
	body := "Thanks for visiting!\n"
	fmt.Fprintf(w, "%s", body)
}

func vectorAdd(v1, v2 []float64) ([]float64, error) {
	if len(v1) != len(v2) {
		return nil, fmt.Errorf("vectors must be of the same length")
	}
	result := make([]float64, len(v1))
	for i := range v1 {
		result[i] = v1[i] + v2[i]
	}
	return result, nil
}

func vectorSub(v1, v2 []float64) ([]float64, error) {
	if len(v1) != len(v2) {
		return nil, fmt.Errorf("vectors must be of the same length")
	}
	result := make([]float64, len(v1))
	for i := range v1 {
		result[i] = v1[i] - v2[i]
	}
	return result, nil
}

func matrixAdd(m1, m2 [][]float64) ([][]float64, error) {
	if len(m1) != len(m2) && len(m1[0]) != len(m2[0]) {
		return nil, fmt.Errorf("matrices must be of the same length")
	}
	
	result := make([][]float64, len(m1))
	for i := range m1 {
		result[i] = make([]float64, len(m1[0])) // Initialize inner slice
		for j := range m1[0] {
			result[i][j] = m1[i][j] + m2[i][j]
		}
	}
	return result, nil
}

func matrixSub(m1, m2 [][]float64) ([][]float64, error) {
	if len(m1) != len(m2) && len(m1[0]) != len(m2[0]) {
		return nil, fmt.Errorf("matrices must be of the same length")
	}
	
	result := make([][]float64, len(m1))
	for i := range m1 {
		result[i] = make([]float64, len(m1[0])) // Initialize inner slice
		for j := range m1[0] {
			result[i][j] = m1[i][j] - m2[i][j]
		}
	}
	return result, nil
}
func vectorAddHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	d, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error reading request body", http.StatusBadRequest)
		return
	}

	var vectors Vectors
	err = json.Unmarshal(d, &vectors)
	if err != nil {
		log.Println(err)
		http.Error(w, "Error parsing JSON", http.StatusBadRequest)
		return
	}

	addResult, err := vectorAdd(vectors.Vector1, vectors.Vector2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result := Result{
                Exp:    addResult,
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(result)
}

func vectorSubHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	d, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error reading request body", http.StatusBadRequest)
		return
	}

	var vectors Vectors
	err = json.Unmarshal(d, &vectors)
	if err != nil {
		log.Println(err)
		http.Error(w, "Error parsing JSON", http.StatusBadRequest)
		return
	}


	subResult, err := vectorSub(vectors.Vector1, vectors.Vector2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result := Result{
		Exp:    subResult,
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(result)
}

func matrixAddHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	d, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error reading request body", http.StatusBadRequest)
		return
	}

	var matrices Matrices
	err = json.Unmarshal(d, &matrices)
	if err != nil {
		log.Println(err)
		http.Error(w, "Error parsing JSON", http.StatusBadRequest)
		return
	}

	addResult, err := matrixAdd(matrices.Matrix1, matrices.Matrix2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result := MatrixResult{
                MExp:    addResult,
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(result)
}

func matrixSubHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	d, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error reading request body", http.StatusBadRequest)
		return
	}

	var matrices Matrices
	err = json.Unmarshal(d, &matrices)
	if err != nil {
		log.Println(err)
		http.Error(w, "Error parsing JSON", http.StatusBadRequest)
		return
	}

	subResult, err := matrixSub(matrices.Matrix1, matrices.Matrix2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result := MatrixResult{
                MExp:    subResult,
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(result)
}

func main() {
	arguments := os.Args
	if len(arguments) != 1 {
		PORT = ":" + arguments[1]
	}
	mux := http.NewServeMux()
	s := &http.Server{
		Addr:         PORT,
		Handler:      mux,
		IdleTimeout:  10 * time.Second,
		ReadTimeout:  time.Second,
		WriteTimeout: time.Second,
	}
	mux.Handle("/api/vectors/add", http.HandlerFunc(vectorAddHandler))
	mux.Handle("/api/vectors/sub", http.HandlerFunc(vectorSubHandler))
	mux.Handle("/api/matrices/add", http.HandlerFunc(matrixAddHandler))
	mux.Handle("/api/matrices/sub", http.HandlerFunc(matrixSubHandler))
	mux.Handle("/", http.HandlerFunc(defaultHandler))

	fmt.Println("Ready to serve at", PORT)
	err := s.ListenAndServe()
	if err != nil {
		fmt.Println(err)
		return
	}
}
