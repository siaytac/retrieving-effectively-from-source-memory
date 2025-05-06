# Retrieving Effectively from Source Memory
This repository implements the Retrieving Effectively from Source Memory model developed in Aytac et al. (2024, Cognitive Psychology). The model extends the classic REM framework (Shiffrin & Steyvers, 1997) to source memory by incorporating REM’s core principle of differentiation and introducing a local matching process to account for source judgments.

## File Descriptions
- `makevectors.R`: Creates memory vectors.
- `updatevectors.R`: Update memory vectors after learning.
- `recognitiontest.R`: Computes the similarity between the probe and each memory trace.
- `sourcetest.R`: Computes the similarity between the source probe and the sources of the best-matching item trace.
- `memory.R`: Runs the memory tasks, including item and source recognition.
- `runme.R`: Runs model simulations. This is the main script for generating predictions. Parameter values and task design features can be set here.

## Getting Started
To generate predictions, run the runme.R script after adjusting the parameters and task design as needed.

## Related Publication 
Aytaç, S., Kılıç, A., Criss, A. H., & Kellen, D. (2024). Retrieving effectively from source memory: Evidence for differentiation and local matching processes. *Cognitive Psychology, 149*, 101617. https://doi.org/10.1016/j.cogpsych.2023.101617 

## Citation
If you use or refer to this code, please cite:
- See [`CITATION.bib`](./CITATION.bib) for the full reference.
