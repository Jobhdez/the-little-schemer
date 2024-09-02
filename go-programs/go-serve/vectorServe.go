package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"

	_ "github.com/lib/pq"
)

const (
	host     = "localhost"
	port     = 5432
	user     = "golalg"
	password = "hello123"
	dbname   = "golalg"
)

var db *sql.DB

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
	ID      int       `json:"id"`
	Vector1 []float64 `json:"vector1"`
	Vector2 []float64 `json:"vector2"`
	Result  []float64 `json:"result"`
}

type MatrixEntry struct {
	ID      int         `json:"id"`
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

func makeVectorOpHandler(operation func([]float64, []float64) ([]float64, error), tableName string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
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

		result, err := operation(vectors.Vector1, vectors.Vector2)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		insertQuery := fmt.Sprintf(`INSERT INTO %s (vector1, vector2, result) VALUES ($1, $2, $3)`, tableName)
		_, err = db.Exec(insertQuery, toJSONString(vectors.Vector1), toJSONString(vectors.Vector2), toJSONString(result))
		if err != nil {
			http.Error(w, "Error inserting into database", http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(Result{Exp: result})
	}
}

func makeMatrixOpHandler(operation func([][]float64, [][]float64) ([][]float64, error), tableName string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
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

		result, err := operation(matrices.Matrix1, matrices.Matrix2)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		insertQuery := fmt.Sprintf(`INSERT INTO %s (matrix1, matrix2, result) VALUES ($1, $2, $3)`, tableName)
		_, err = db.Exec(insertQuery, toJSONString2D(matrices.Matrix1), toJSONString2D(matrices.Matrix2), toJSONString2D(result))
		if err != nil {
			http.Error(w, "Error inserting into database", http.StatusInternalServerError)
			return
		}

		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(MatrixResult{MExp: result})
	}
}

func makeGetAllHandler(query string, scanFunc func(*sql.Rows) (interface{}, error)) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("Serving:", r.URL.Path, "from", r.Host, r.Method)
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed!", http.StatusMethodNotAllowed)
			return
		}

		rows, err := db.Query(query)
		if err != nil {
			http.Error(w, "Error querying database", http.StatusInternalServerError)
			return
		}
		defer rows.Close()

		var entries []interface{}
		for rows.Next() {
			entry, err := scanFunc(rows)
			if err != nil {
				http.Error(w, "Error scanning database row", http.StatusInternalServerError)
				return
			}
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
}

func scanVectorEntry(rows *sql.Rows) (interface{}, error) {
	var entry VectorAddEntry
	var vector1, vector2, result string

	err := rows.Scan(&entry.ID, &vector1, &vector2, &result)
	if err != nil {
		return nil, err
	}

	entry.Vector1 = fromJSONString(vector1)
	entry.Vector2 = fromJSONString(vector2)
	entry.Result = fromJSONString(result)
	return entry, nil
}

func scanMatrixEntry(rows *sql.Rows) (interface{}, error) {
	var entry MatrixEntry
	var matrix1, matrix2, result string

	err := rows.Scan(&entry.ID, &matrix1, &matrix2, &result)
	if err != nil {
		return nil, err
	}

	entry.Matrix1 = fromJSONString2D(matrix1)
	entry.Matrix2 = fromJSONString2D(matrix2)
	entry.Result = fromJSONString2D(result)
	return entry, nil
}

/* === Linear Algebra === */

func vectorAdd(v1, v2 []float64) ([]float64, error) {
	if len(v1) != len(v2) {
		return nil, fmt.Errorf("vectors must have the same length")
	}
	result := make([]float64, len(v1))
	for i := range v1 {
		result[i] = v1[i] + v2[i]
	}
	return result, nil
}

func vectorSub(v1, v2 []float64) ([]float64, error) {
	if len(v1) != len(v2) {
		return nil, fmt.Errorf("vectors must have the same length")
	}
	result := make([]float64, len(v1))
	for i := range v1 {
		result[i] = v1[i] - v2[i]
	}
	return result, nil
}

func matrixAdd(m1, m2 [][]float64) ([][]float64, error) {
	if len(m1) != len(m2) || len(m1[0]) != len(m2[0]) {
		return nil, fmt.Errorf("matrices must have the same dimensions")
	}
	result := make([][]float64, len(m1))
	for i := range m1 {
		result[i] = make([]float64, len(m1[i]))
		for j := range m1[i] {
			result[i][j] = m1[i][j] + m2[i][j]
		}
	}
	return result, nil
}

/* === Polynomials === */
// not yet implemented ...

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
	psqlInfo := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=disable",
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

	createTables := []string{
		`CREATE TABLE IF NOT EXISTS vector_add (
			id SERIAL PRIMARY KEY,
			vector1 TEXT NOT NULL,
			vector2 TEXT NOT NULL,
			result TEXT NOT NULL
		)`,
		`CREATE TABLE IF NOT EXISTS vector_sub (
			id SERIAL PRIMARY KEY,
			vector1 TEXT NOT NULL,
			vector2 TEXT NOT NULL,
			result TEXT NOT NULL
		)`,
		`CREATE TABLE IF NOT EXISTS matrix_add2 (
			id SERIAL PRIMARY KEY,
			matrix1 TEXT NOT NULL,
			matrix2 TEXT NOT NULL,
			result TEXT NOT NULL
		)`,
	}

	for _, createTableQuery := range createTables {
		_, err = db.Exec(createTableQuery)
		if err != nil {
			log.Fatal("Error creating table:", err)
		}
	}

	if len(os.Args) > 1 {
		PORT = ":" + os.Args[1]
	}

	mux := http.NewServeMux()
	s := &http.Server{
		Addr:         PORT,
		Handler:      mux,
		IdleTimeout:  10 * time.Second,
		ReadTimeout:  time.Second,
		WriteTimeout: time.Second,
	}

	vectorAddHandler := makeVectorOpHandler(vectorAdd, "vector_add")
	vectorSubHandler := makeVectorOpHandler(vectorSub, "vector_sub")
	matrixAddHandler := makeMatrixOpHandler(matrixAdd, "matrix_add2")
	
	vectorGetAllHandler := makeGetAllHandler("SELECT * FROM vector_add", scanVectorEntry)
	matrixGetAllHandler := makeGetAllHandler("SELECT * FROM matrix_add2", scanMatrixEntry)

	mux.Handle("/api/vector/add", vectorAddHandler)
	mux.Handle("/api/vector/sub", vectorSubHandler)
	mux.Handle("/api/vector/add/vectors", vectorGetAllHandler)
	mux.Handle("/api/vector/sub/vectors", vectorGetAllHandler)

	mux.Handle("/api/matrix/add", matrixAddHandler)
	mux.Handle("/api/matrix/add/matrices", matrixGetAllHandler)


	mux.HandleFunc("/", defaultHandler)

	log.Println("Listening on port", PORT)
	log.Fatal(s.ListenAndServe())
}
