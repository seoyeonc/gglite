# gglite <img src="https://img.shields.io/badge/R-%3E=3.6-276DC3" alt="R >= 3.6" align="right" />

Lightweight ggplot2 helpers for rapid exploratory graphics. gglite bundles a
theming shortcut together with concise wrappers for the geoms highlighted in
Posit's *Data Visualization with ggplot2* guide, so you can move from raw data
to publication-ready figures with just a few lines of code.

---

## 📦 Overview
- **Consistent style**: Start every plot with `gglite()` to apply a clean,
  publication-friendly theme.
- **Cheat-sheet inspired API**: Functions mirror the structure of Posit's
  visualisation cheat sheet (one variable, continuous × continuous, discrete ×
  continuous).
- **Data-friendly inputs**: Accepts vectors, matrices, or tidy data frames and
  handles reshaping under the hood.
- **Notebook ready**: Works smoothly inside Quarto, R Markdown, or interactive
  notebooks.

## 🚀 Installation
```r
# install.packages("devtools") if needed
devtools::install_github("seoyeonc/gglite")
```

## 🏁 Quick start
```r
library(gglite)
library(ggplot2)

# Continuous × continuous
p1 <- gglite(mtcars, aes(wt, mpg)) +
  point() +
  smooth(se = FALSE)

# One variable (continuous)
p2 <- gglite() +
  density(list(compact = rnorm(200), premium = rnorm(200, mean = 0.5)))

# Discrete × continuous
p3 <- gglite() +
  boxplot(y = iris$Sepal.Length, label = iris$Species)

p1
p2
p3
```

## 🧭 Function map
| Category | Helpers |
| --- | --- |
| **Theme starter** | `gglite()` |
| **One variable (continuous)** | `histogram()`, `density()`, `qq()`, `qq_line()` |
| **Two variables (continuous × continuous)** | `point()`, `line()`, `smooth()`, `step()` |
| **Two variables (discrete × continuous)** | `col()`, `boxplot()`, `violin()`, `jitter()` |

Call `?gglite-package` or `?line` inside R for full documentation and examples.

## 🛠 Development
```r
# Run package checks
devtools::check()

# Update documentation after editing roxygen comments
devtools::document()
```

## 📚 Citation
If you use gglite in your work, please cite:

> Seoyeon Choi & Guebin Choi (2025). *gglite: Lightweight helpers for ggplot2*.
> Version 0.1.0. Department of Statistics (Institute of Applied Statistics),
> Jeonbuk National University.

BibTeX entry:
```bibtex
@manual{gglite2025,
  title        = {gglite: Lightweight helpers for ggplot2},
  author       = {Seoyeon Choi and Guebin Choi},
  year         = {2025},
  note         = {R package version 0.1.0},
  organization = {Department of Statistics (Institute of Applied Statistics), Jeonbuk National University},
  url          = {https://github.com/seoyeonc/gglite}
}
```

## 📄 License
This project is released under the MIT License. See [`LICENSE`](LICENSE) for
details.

## 🙏 Acknowledgements
- Inspired by Posit's *Data Visualization with ggplot2* (2022).
- Built on top of the amazing [`ggplot2`](https://ggplot2.tidyverse.org/)
  ecosystem.

## 🤝 Contributing
Issues, suggestions, and pull requests are welcome. If you plan a larger
contribution, please open an issue first so we can align on direction.
