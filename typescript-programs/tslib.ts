// simple linear algebra
// this file exists as a future reference for myself :-)
function elementWisev(fn: (a: number, b: number) => number) {
  return (v: number[], v2: number[]) => v.map((x, i) => fn(x, v2[i]));
}

function elementWisem(fn: (a: number[], b: number[]) => number[]) {
  return (m: number[][], m2: number[][]) => m.map((x, i) => fn(x, m2[i]));
}

const add = (e: number, e2: number): number => {
  return e + e2;
};

export const addv = elementWisev(add);
export const addm = elementWisem(addv);
