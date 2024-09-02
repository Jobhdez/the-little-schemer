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

type MatrixEntry struct {
	ID      int      `json:"id"`
	Matrix1 [][]float64 `json:"matrix1"`
	Matrix2 [][]float64 `json:"matrix2"`
	Result  [][]float64 `json:"result"`
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

func getAllVectorSubssHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	rows, err := db.Query("SELECT id, vector1, vector2, result FROM vector_sub")
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

	insertQuery := `INSERT INTO vector_sub (vector1, vector2, result) VALUES ($1, $2, $3)`
	_, err = db.Exec(insertQuery, toJSONString(vectors.Vector1), toJSONString(vectors.Vector2), toJSONString(subResult))
	if err != nil {
		http.Error(w, "Error inserting into database", http.StatusInternalServerError)
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
	
	insertQuery := `INSERT INTO matrix_add2 (matrix1, matrix2, result) VALUES ($1, $2, $3)`
	_, err = db.Exec(insertQuery, toJSONString2D(matrices.Matrix1), toJSONString2D(matrices.Matrix2), toJSONString2D(addResult))
	if err != nil {
		http.Error(w, "Error inserting into database", http.StatusInternalServerError)
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

func getAllMatrixAddsHandler(w http.ResponseWriter, r *http.Request) {
	log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
		return
	}

	rows, err := db.Query("SELECT id, matrix1, matrix2, result FROM matrix_add2")
	if err != nil {
		http.Error(w, "Error querying database", http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var entries []MatrixEntry
	for rows.Next() {
		var entry MatrixEntry
		var matrix1, matrix2, result string

		err := rows.Scan(&entry.ID, &matrix1, &matrix2, &result)
		if err != nil {
			http.Error(w, "Error scanning database row", http.StatusInternalServerError)
			return
		}

		entry.Matrix1 = fromJSONString2D(matrix1)
		entry.Matrix2 = fromJSONString2D(matrix2)
		entry.Result = fromJSONString2D(result)

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

/* === Utils ==== */
func toJSONString(data interface{}) string {
	bytes, _ := json.Marshal(data)
	return string(bytes)
}

func fromJSONString(data string) []float64 {
	var result []float64
	json.Unmarshal([]byte(data), &result)
	return result
}

func toJSONString2D(data [][]float64) string {
	bytes, _ := json.Marshal(data)
	return string(bytes)
}


func fromJSONString2D(data string) [][]float64 {
	var result [][]float64
	json.Unmarshal([]byte(data), &result)
	return result
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

	createTableQuery2 := `CREATE TABLE IF NOT EXISTS vector_sub (
		id SERIAL PRIMARY KEY,
		vector1 TEXT NOT NULL,
		vector2 TEXT NOT NULL,
		result TEXT NOT NULL
	)`
	_, err = db.Exec(createTableQuery2)
	if err != nil {
		log.Fatal("Error creating table:", err)
	}

	createTableQuery3 := `CREATE TABLE IF NOT EXISTS matrix_add2 (
		id SERIAL PRIMARY KEY,
		matrix1 TEXT NOT NULL,
		matrix2 TEXT NOT NULL,
		result TEXT NOT NULL
	)`
	_, err = db.Exec(createTableQuery3)
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
	mux.Handle("/api/vector/add", http.HandlerFunc(vectorAddHandler))
	mux.Handle("/api/vector/sub", http.HandlerFunc(vectorSubHandler))
	mux.Handle("/api/matrix/add", http.HandlerFunc(matrixAddHandler))
	mux.Handle("/api/matrix/sub", http.HandlerFunc(matrixSubHandler))
	mux.Handle("/api/vector/add/vectors", http.HandlerFunc(getAllVectorAddsHandler))
	mux.Handle("/api/vector/sub/vectors", http.HandlerFunc(getAllVectorAddsHandler))
	mux.Handle("/api/matrix/add/matrices", http.HandlerFunc(getAllMatrixAddsHandler))

	mux.Handle("/", http.HandlerFunc(defaultHandler))

	fmt.Println("Ready to serve at", PORT)
	err = s.ListenAndServe()
	if err != nil {
		fmt.Println(err)
		return
	}
}
