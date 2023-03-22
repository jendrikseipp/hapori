# [Deep Learning for Cost-Optimal Planning: Task-Dependent Planner Selection](https://ojs.aaai.org/index.php/AAAI/article/view/4767)

**Silvan Sievers, Michael Katz, Shirin Sohrabi, Horst Samulowitz and Patrick Ferber**

This directory contains the code and input data for the above mentioned 
[AAAI 2019](https://aaai.org/Conferences/AAAI-19/) paper.

## Setup

1. The code still requires **Python 2**! 
2. Create and activate a virtual Python 2 environment, e.g. 
   ```
   virtualenv -p python2 venv
   source venc/bin/activate
   ```
3. Install the required Python packages
   ```
   pip install -r requirements.txt
   ```

## Execute

Execute `run.sh` to see an example call and to test if your setup worked.

- `image_based_network.py`: the main script for training.
- `data`: main directory containing the training data.
