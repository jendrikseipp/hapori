from data_process import get_data
from mip import mip
import time

init_time = time.time()
# Folder that contains the extracted property.xz file(s)
data_folder = 'data_folder'
track = 'opt' #agl, sat, opt
data_dictionary, solvers, tasks = get_data(data_folder, track)
end_time = time.time()
print(f'Data preprocess done in {end_time - init_time}')
init_time = time.time()
mip(data_dictionary, solvers, tasks, track)
end_time = time.time()
print(f'MIP done in {end_time - init_time}')
