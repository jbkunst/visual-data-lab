A closed path can be treated as a complex signal:

\\[
z(t) = x(t) + i y(t)
\\]

R samples the selected shape at equal distances and applies the discrete Fourier transform. Each coefficient becomes one rotating vector:

- its **amplitude** is the circle radius;
- its **frequency** controls rotation speed and direction;
- its **phase** sets the initial angle.

The vectors are added tip to tail. The final tip traces the reconstructed shape. Increasing the number of circles preserves more detail, while the reconstruction panel compares the result with the original path.

The selected coefficients are always the largest by amplitude. **Circle order** only changes how the vector chain is displayed; it does not change the final reconstruction.

The canvas animation is a small native JavaScript implementation inspired by [The Coding Train's Fourier epicycles tutorial](https://thecodingtrain.com/challenges/130-drawing-with-fourier-transform-and-epicycles/) and the visual explanation in [3Blue1Brown's Fourier series lesson](https://www.3blue1brown.com/lessons/fourier-series).
