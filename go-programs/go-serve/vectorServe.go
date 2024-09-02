package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"
	"database/sql"
	_ "github.com/lib/pq"
)

const (
	host     = "localhost"
	port     = 5432
	user     = "golalg"
	password = "hello123"
	dbname   = "golalg"
)

var (
	db   *sql.DB
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

type VectorAddEntry struct {
	ID      int      `json:"id"`
	Vector1 []float64 `json:"vector1"`
	Vector2 []float64 `json:"vector2"`
	Result  []float64 `json:"result"`
}

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
		result[i] = make([]float64, len(m1[0])) 
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
		result[i] = make([]float64, len(m1[0])) 
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

	insertQuery := `INSERT INTO vector_add (vector1, vector2, result) VALUES ($1, $2, $3)`
	_, err = db.Exec(insertQuery, toJSONString(vectors.Vector1), toJSONString(vectors.Vector2), toJSONString(addResult))
	if err != nil {
		http.Error(w, "Error inserting into database", http.StatusInternalServerError)
		return
	}

	result := Result{
		Exp: addResult,
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(result)
}

func getAllVectorAddsHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	rows, err := db.Query("SELECT id, vector1, vector2, result FROM vector_add")
	if err != nil {
		http.Error(w, "Error querying database", http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var entries []VectorAddEntry
	for rows.Next() {
		var entry VectorAddEntry
		var vector1, vector2, result string

		err := rows.Scan(&entry.ID, &vector1, &vector2, &result)
		if err != nil {
			http.Error(w, "Error scanning database row", http.StatusInternalServerError)
			return
		}

		entry.Vector1 = fromJSONString(vector1)
		entry.Vector2 = fromJSONString(vector2)
		entry.Result = fromJSONString(result)

		entries = append(entries, entry)
	}

	if err = rows.Err(); err != nil {
		http.Error(w, "Error iterating over rows", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(entries)
}

func toJSONString(data interface{}) string {
	bytes, _ := json.Marshal(data)
	return string(bytes)
}

func fromJSONString(data string) []float64 {
	var result []float64
	json.Unmarshal([]byte(data), &result)
	return result
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
	psqlInfo := fmt.Sprintf("host=%s port=%d user=%s "+
		"password=%s dbname=%s sslmode=disable",
		host, port, user, password, dbname)
	var err error
	db, err = sql.Open("postgres", psqlInfo)
	if err != nil {
		log.Fatal("Error opening database:", err)
	}
	defer db.Close()

	err = db.Ping()
	if err != nil {
		log.Fatal("Error connecting to database:", err)
	}

	log.Println("Successfully connected to database!")

	
	createTableQuery := `CREATE TABLE IF NOT EXISTS vector_add (
		id SERIAL PRIMARY KEY,
		vector1 TEXT NOT NULL,
		vector2 TEXT NOT NULL,
		result TEXT NOT NULL
	)`
	_, err = db.Exec(createTableQuery)
	if err != nil {
		log.Fatal("Error creating table:", err)
	}
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
	mux.Handle("/api/vectors", http.HandlerFunc(getAllVectorAddsHandler))
	mux.Handle("/", http.HandlerFunc(defaultHandler))

	fmt.Println("Ready to serve at", PORT)
	err = s.ListenAndServe()
	if err != nil {
		fmt.Println(err)
		return
	}
}
