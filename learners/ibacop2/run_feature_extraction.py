import os 

path_to_benchmarks = '../../benchmarks'
path_to_ftdata = 'train_data/benchmark_features/'
# path_to_ftdata = 'train_data/dev_features/'

if __name__ == '__main__':

    for dom_folder in os.listdir(path_to_benchmarks):
        dom_path = os.path.join(path_to_benchmarks, dom_folder)
        data_path = os.path.join(path_to_ftdata, f'{dom_folder}_features.csv')
        if not os.path.exists(data_path):
            # collecting features for this domain
            cmd = f'python ibafeatures_on_img.py -p {dom_path} -o {data_path} -m'
            print(cmd)
            print()
            os.system(cmd)
        else:
            print(f"Features for {dom_folder} already exists. Skipping... ")
    