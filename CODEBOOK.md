# Codebook

Documents the variables retained in the two data files used by `multilingualism_analysis.Rmd`. Both files contain only the variables needed to reproduce the analysis and figures; all respondent-identifying information (names, free-text answers, timestamps, etc.) has been removed.

## `multiling_variables.csv`

433 respondents. Used to produce Figures 1, 2, 3, 4 and 5.

| Variable | Description | Values |
|---|---|---|
| `text_lang` | Language(s) of the text material the respondent studies | `English`, `Other than English`, `Multilingual` |
| `CONval` | Concern for validity of computational text analysis methods (CTAM) | 0 (lowest concern) – 8 (highest concern) |
| `CONava` | Concern for availability of suitable CTAM | 0 (lowest concern) – 6 (highest concern) |
| `Quant_CTAM` | Use of computational text analysis | `1` = Don't use it, `2` = Only use it when collaborating, `3` = Rarely use it, `4` = Regularly use it, `5` = Primarily use it |
| `COMP` | Whether the respondent conducts inter-lingually comparative research | `0` = No, `1` = Yes |
| `spoken_lang` | Respondent's native-language status | `0` = Native English speaker, `1` = Non-native English speaker |
| `VALn` | Number of distinct strategies used to validate CTAM output | 0 – 5 |

## `data_cleaned_labeled.csv`

426 respondents. Used to produce Figure 6.

Q7_1 through Q7_8 ask respondents to rate potential reasons for not studying additional languages beyond those they already work with, as either a major reason, a minor reason, or not applicable to them.
Respondents who do not conduct multilingual research at all are coded `No multiple languages` for every item.

| Variable | Reason presented to respondent |
|---|---|
| `Q7_1` | I don't speak these languages well enough |
| `Q7_2` | It is less relevant for my research/not worth the additional effort |
| `Q7_3` | It is difficult to access textual material in these languages |
| `Q7_4` | It is difficult to find qualified collaborators/assistants for these languages |
| `Q7_5` | It is difficult to find suitable tools for these languages |
| `Q7_6` | The quality of machine translation is insufficient for these languages |
| `Q7_7` | It is difficult to compare between languages |
| `Q7_8` | It is difficult to finance a multilingual project |

Values for `Q7_1`–`Q7_8`: `Major reason`, `Minor reason`, `No reason`, `No multiple languages`.
