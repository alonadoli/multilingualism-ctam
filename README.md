# Multilingualism & Computational Text Analysis

Research compendium for the paper:

> Dolinsky, A. O., Schoonvelde, M., Baden, C., Lind, F., Pipal, C., Shababo, G., van der Velden, M. A. C. G., & Zalik, A. (forthcoming). Challenges for multilingual computational text analysis researchers: evidence from a survey of social scientists. *Acta Politica*.
> (DOI to be added upon publication.)

## Authors

| Author | Affiliation | Email |
|---|---|---|
| Alona O. Dolinsky | School of Politics and International Relations, University College Dublin, Dublin, Ireland | alona.dolinsky1@ucd.ie |
| Martijn Schoonvelde | University of Groningen, Groningen, The Netherlands | martijn.schoonvelde@rug.nl |
| Christian Baden | Department of Communication and Journalism, Hebrew University of Jerusalem, Jerusalem, Israel | c.baden@mail.huji.ac.il |
| Fabienne Lind | Department of Communication, University of Vienna, Vienna, Austria | fabienne.lind@univie.ac.at |
| Christian Pipal | Department of Communication and Media Research, University of Zurich, Zurich, Switzerland | c.pipal@ikmz.uzh.ch |
| Guy Shababo | Department of East Asia Studies, Hebrew University of Jerusalem, Jerusalem, Israel | guy.shababo@gmail.com |
| Mariken A. C. G. van der Velden | Department of Communication Science, Vrije Universiteit Amsterdam, Amsterdam, The Netherlands | m.a.c.g.vander.velden@vu.nl |
| Avital Zalik | Department of Communication and Journalism, Hebrew University of Jerusalem, Jerusalem, Israel | avital.zalik@mail.huji.ac.il |

## Funding

This research was funded by the European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme, grant agreement No. 951832, OPTED (www.opted.eu).

## Data and Code

* `multiling_variables.csv` — survey data used to produce Figures 1–5.
* `data_cleaned_labeled.csv` — survey data used to produce Figure 6.
* `multilingualism_analysis.Rmd` — full replication script; reproduces all six figures reported in the paper and saves them to `plots/`.

See `CODEBOOK.md` for variable definitions and value labels. Both CSV files have been reduced to only the variables required to reproduce the analysis; no respondent-identifying information is included.

## How to Reproduce

1. Open `multilingualism-ctam.Rproj` in RStudio (this sets the working directory correctly for the relative file paths used throughout).
2. Knit `multilingualism_analysis.Rmd`. It reads the two CSV files above, and regenerates the six figures into `plots/`.

## License

This repository is released under the CC0 1.0 Universal license — see `LICENSE`.
