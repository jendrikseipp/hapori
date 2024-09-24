#! /bin/bash

set -euo pipefail

# Change into current directory.
cd "$(dirname "$0")"

#rsync -avh --progress 2024-*-*.sif nsc:/proj/dfsplan/users/x_jense/hapori/
rsync -avh --progress 2024-*-*.sif login-infai:~/projects/hapori/
