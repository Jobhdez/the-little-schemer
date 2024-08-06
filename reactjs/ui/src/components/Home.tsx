// App.tsx
import React from 'react';
import { Button, Grid, Card, CardActions, CardContent, Typography, Box } from '@mui/material';

const Home = () => {

  return (
    <Box p={40}>
      <Grid container spacing={2} justifyContent="center">
        <Grid item xs={12} sm={6}>
          <Card sx={{ width: 500, maxWidth: "100%" }}>
            
              <CardContent>
                <Typography variant="h5" component="div">
                  Matrices
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Explore matrix arithmetic
                </Typography>
              </CardContent>
              <CardActions>
                <Button variant="contained" href="matrix">
                  Matrix calculator
                </Button>
              </CardActions>
            
          </Card>
        </Grid>
        <Grid item xs={12} sm={6}>
          <Card sx={{ width: 500, maxWidth: "100%" }}>
            
              <CardContent>
                <Typography variant="h5" component="div">
                  Vectors
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Explore vector arithmetic
                </Typography>
              </CardContent>
            <CardActions>
              <Button variant="contained" href="vector">Vector Calculator</Button>
            </CardActions>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
};


export default Home;
