A Fourier series represents a function as a weighted sum of sine and cosine waves.

Choose a target function on the interval **0 to 10** and change the number of components used in its reconstruction.

- **Function and approximation** compares the target with the current Fourier reconstruction.
- **Leading Fourier components** shows up to five of the strongest waves on the same axis. Line width increases with component amplitude.
- **Fourier spectrum** summarizes the amplitude associated with every included frequency.
- **Reconstruction quality** shows how much variation is recovered as components are added. The tooltip also reports the marginal gain from the latest component.

The periodic signal is built from only three frequencies, so it can be reconstructed exactly with a few terms. The floor function contains jumps and therefore needs many higher frequencies. Oscillations near a jump are expected and illustrate the Gibbs phenomenon.

Fourier series repeat the selected interval periodically. When the function does not join smoothly at the two boundaries, the periodic extension creates an additional discontinuity.
