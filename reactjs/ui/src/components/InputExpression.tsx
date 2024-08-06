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
import "../assets/styles.css";

const InputExpression = () => {
  const [vector1, setVector1] = React.useState<string>("");
  const [vector2, setVector2] = React.useState<string>("");
  const [mathExp, setMathExp] = React.useState<string>("");

  const compileToMathJax = (operation: string) => {
    const parseVector = (vector: string) => {
      return vector
        .replace(/[\[\]]/g, "")
        .trim()
        .split(/\s+/)
        .map(Number);
    };

    const vec1 = parseVector(vector1);
    const vec2 = parseVector(vector2);

    let op: string;
    switch (operation) {
      case "add":
        op = "+";
        break;
      case "subtract":
        op = "-";
        break;
      case "dot-product":
        op = "*";
        break;
      default:
        op = "+";
    }

    setMathExp(
      `$$\\begin{pmatrix} ${vec1.join(" & ")} \\end{pmatrix} ${op} \\begin{pmatrix} ${vec2.join(" & ")} \\end{pmatrix}$$`,
    );
  };

  return (
    <Box p={45}>
      <Card sx={{ width: 500, maxWidth: "100%" }}>
        <CardHeader title="Vector Operations" />
        <CardContent>
          <Box mb={2}>
            <TextField
              label="[2 3 4 5]"
              id="vector1-input"
              fullWidth
              onChange={(e) => setVector1(e.target.value)}
            />
          </Box>
          <Box mb={2}>
            <TextField
              label="[4 5 6 7]"
              id="vector2-input"
              fullWidth
              onChange={(e) => setVector2(e.target.value)}
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
                <MenuItem value="dot-product">Dot Product</MenuItem>
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
    </Box>
  );
};

export default InputExpression;
