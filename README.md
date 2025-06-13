# Technology Convergence Dynamics
This repository contains data and code associated with the article:
> **Uncovering the Formation Dynamics of Technology Convergence: A Multi-dimensional BTERGM Approach Integrating Static and Temporal Mechanisms**

## Repository Contents
### Data
- Processed U.S. patent data (2011–2020) from PatentsView.
- Annual CPC-based technology convergence networks.
### Code
- Scripts for data extraction and preprocessing.
- Network construction and statistical analysis.
- BTERGM modeling and result visualization.

## Computational Environment & Runtime
### Software
| Component | Version / Build |
|-----------|-----------------|
| **R** | 4.4.2 (2024-10-31 ucrt) |
| **RStudio** | 2024.04 (Desktop) |
| **btergm** | 1.10.12 |
| **ergm** | 4.7.5 |
| **igraph** | 2.1.2 |
| Other key deps | `Matrix 1.7-1`, `sna 2.8`, `dplyr 1.1.4`, etc.|

### Hardware
* **OS / Platform** : Windows 11 x64 (build 26100), `x86_64-w64-mingw32/x64`  
* **CPU** : 16 physical cores / 22 logical cores  
* **GPU** : _Not used_; all BTERGM estimations run on CPU  
* **Parallel backend** : `parallel = "snow"` with `ncpus = 5`

### BTERGM Estimation Time
| Model | Runtime (seconds) | Runtime (minutes) |
|-------|-------------------|-------------------|
| M1 | 207.23 | 3.5 |
| M2 | 1149.82 | 19.2 |
| M3 | 1419.83 | 23.7 |
| M4 | 2582.86 | 43.0 |
| M5 | 999.24 | 16.7 |
| M6 | 3873.53 | 64.6 |
| M7 | 3824.78 | 63.7 |
| M8 | 3963.82 | 66.1 |
| M9 | 3802.88 | 63.4 |
| M10 | 4754.26 | 79.2 |
| M11 | 3909.66 | 65.2 |
| M12 | 3377.38 | 56.3 |
| M13 | 3278.00 | 54.6 |

> **Note**   
> *Times were recorded with `proc.time()` calls surrounding each `btergm()` invocation.  
> All runs used five worker processes; wall-clock times include data loading and bootstrap resampling overhead.*

## Citation and Licensing
Please cite our work if you use this repository for your research:

> X.Lu, Y.Yan, G.Yang, J.Xing. (2025). Uncovering the Formation Dynamics of Technology Convergence: A Multi-dimensional BTERGM Approach Integrating Static and Temporal Mechanisms. [Information Processing & Management, under review].

**Note**: This repository is currently shared for research replication purposes. The final citation details will be updated once the article is published.

This project is licensed under the MIT License.

## Contact
For inquiries or contributions, please contact [Yicong Yan] at [ycyan@ruc.edu.cn].
