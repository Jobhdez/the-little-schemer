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

type Result struct {
	Addition       []float64 `json:"addition"`
	Subtraction    []float64 `json:"subtraction"`
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

func vectorHandler(w http.ResponseWriter, r *http.Request) {
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

	subResult, err := vectorSub(vectors.Vector1, vectors.Vector2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result := Result{
		Addition:    addResult,
		Subtraction: subResult,
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
	mux.Handle("/vectors", http.HandlerFunc(vectorHandler))
	mux.Handle("/", http.HandlerFunc(defaultHandler))

	fmt.Println("Ready to serve at", PORT)
	err := s.ListenAndServe()
	if err != nil {
		fmt.Println(err)
		return
	}
}
