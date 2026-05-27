# Symmetro

An interactive Ising model simulator written in Elm.

Symmetro visualizes:

- spontaneous symmetry breaking
- domain formation
- thermal fluctuations
- critical behavior near the 2D Ising transition

The simulation uses asynchronous Metropolis updates and periodic boundary conditions.

---

## Features

- Interactive temperature control
- Real-time Monte Carlo dynamics
- Magnetization visualization
- Critical temperature shortcut
- Adjustable simulation speed
- Responsive dark-mode UI

---

## Physics

The simulator implements the ferromagnetic 2D Ising model:

E = -Σ s_i s_j

with Metropolis-Hastings dynamics.

The exact critical temperature for the square lattice is:

Tc = 2 / ln(1 + √2) ≈ 2.269

Below this temperature, spontaneous magnetization emerges.

---

## Development

Install Elm:

```bash
npm install -g elm
```
