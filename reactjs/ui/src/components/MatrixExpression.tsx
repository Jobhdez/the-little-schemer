import * as React from "react";
import Box from "@mui/material/Box";
import TextField from "@mui/material/TextField";
import InputLabel from "@mui/material/InputLabel";
import MenuItem from "@mui/material/MenuItem";
import FormControl from "@mui/material/FormControl";
import { MathJaxProvider, MathJaxFormula } from "mathjax3-react";
import Select from "@mui/material/Select";
import Card from "@mui/material/Card";
import CardContent from "@mui/material/CardContent";
import CardHeader from "@mui/material/CardHeader";
import Grid from "@mui/material/Grid";

const MatrixExpression = () => {
  const [matrix1, setMatrix1] = React.useState<string>("");
  const [matrix2, setMatrix2] = React.useState<string>("");
  const [mathExp, setMathExp] = React.useState<string>("");

  const compileToMathJax = (operation: string) => {
    const parseMatrix = (matrix: string) => {
      return matrix
        .trim()
        .split(/[\[\]]+/)
        .filter((row) => row.trim() !== "")
        .map((row) =>
          row
            .trim()
            .split(/\s+/)
            .map(Number)
        );
    };

    const mat1 = parseMatrix(matrix1);
    const mat2 = parseMatrix(matrix2);

    let op: string;
    switch (operation) {
      case "add":
        op = "+";
        break;
      case "subtract":
        op = "-";
        break;
      case "multiply":
        op = "*";
        break;
      default:
        op = "+";
    }

    const matrixToString = (matrix: number[][]) =>
      matrix.map((row) => row.join(" & ")).join(" \\\\ ");

    setMathExp(
      `$$\\begin{pmatrix} ${matrixToString(mat1)} \\end{pmatrix} ${op} \\begin{pmatrix} ${matrixToString(mat2)} \\end{pmatrix}$$`
    );
  };

  return (
  
    <Card sx={{ width: 500, maxWidth: "100%" }}>
      <CardHeader title="Matrix Operations" />
      <CardContent>
        <Box mb={2}>
          <TextField
            label="[[4 5 6 7][6 7 8 9]]"
            id="matrix1-input"
            fullWidth
            onChange={(e) => setMatrix1(e.target.value)}
          />
        </Box>
        <Box mb={2}>
          <TextField
            label="[[5 6 7 7][7 8 9 0]]"
            id="matrix2-input"
            fullWidth
            onChange={(e) => setMatrix2(e.target.value)}
          />
        </Box>
        <Box mb={2}>
          <FormControl fullWidth>
            <InputLabel id="operator-select-label">Operator</InputLabel>
            <Select
              labelId="operator-select-label"
              id="operator-select"
              label="Operation"
              onChange={(e) => compileToMathJax(e.target.value as string)}
            >
              <MenuItem value="add">Add</MenuItem>
              <MenuItem value="subtract">Subtract</MenuItem>
              <MenuItem value="multiply">Multiply</MenuItem>
            </Select>
          </FormControl>
        </Box>
        <Box mt={2}>
          <MathJaxProvider>
            <MathJaxFormula formula={mathExp} />
          </MathJaxProvider>
        </Box>
      </CardContent>
    </Card>
  );
};

export default MatrixExpression;
