from util import gen_fun_matrix, evolve_var_system
import numpy as np
import pickle
from itertools import islice
from scipy.stats import special_ortho_group, ortho_group
random = np.random.RandomState(1339)
np.set_printoptions(suppress=True)
def find_test_matrix(K=4, threshold=0.5, tries=100):

    def mat_has_all_comensalisms(mat):
        full_comp = False
        partial_comp = False
        pred = False
        partial_mut = False
        full_mut = False
        neutral = False

        is_full_comp = lambda a, b: (a < -1 * threshold) and  (b < -1 * threshold)
        is_pred = lambda a, b: ((a < -1 * threshold) and  (b > threshold)) or ((a > threshold) and  (b < -1 * threshold))
        is_partial_comp = lambda a, b: not is_full_comp(a,b) and not is_pred(a,b) and ((a < -1 * threshold) or (b < -1 * threshold))
        is_full_mut =  lambda a, b: (a > threshold) and  (b > threshold)
        is_partial_mut = lambda a, b: not is_full_mut(a,b) and not is_pred(a,b) and ((a > threshold) or (b > threshold))
        is_neutral = lambda a, b: not is_full_comp(a,b) and not is_pred(a,b) and not is_partial_comp(a, b) and not is_full_mut(a, b) and not is_partial_mut(a,b)
        
        for i in range(mat.shape[0]):
            for j in range(mat.shape[1]):
                if i != j:
                    a = mat[i,j]
                    b = mat[j,i]
                    if is_full_comp(a,b):
                        full_comp = True
                    if is_pred(a,b):
                        pred = True
                    if is_partial_comp(a,b):
                        partial_comp = True
                    if is_full_mut(a,b):
                        full_mut = True
                    if is_partial_mut(a,b):
                        partial_mut = True
                    if is_neutral(a,b):
                        neutral = True

        return all([full_comp, partial_comp, pred, partial_mut, full_mut, neutral])


    # def random_unif_complex():
    #     while True:
    #         val = np.complex(np.random.uniform(-1,1),np.random.uniform(-1,1))
    #         if np.abs(val) < 1:
    #             yield val
    
    def gen_matrix(K):

        if K == 6:
            mat = np.matrix([[0.4,0.4,0,0,0,-0.4],[-0.4,0.7,0,0,-0.4,0],[0,0,0.5,0.4,0,0],[0,0,0.4,0.4,0,0],[0,0.4,0,0,0.5,0],[-0.4,0,0,0,0,0.8]])

            np.abs(np.linalg.eig(mat)[0])

        else:
            eigens = np.random.uniform(-1.0,0.49,K) + 0.5
            Q, _ = np.linalg.qr(np.random.rand(K, K))
            mat = Q.T @ np.diag(eigens) @ Q
        return(mat)
        
    min_n_non_sparse = K**2
    min_mat = None
    for i in range(tries):
        mat = gen_matrix(K)
        if min_mat is None:
            min_mat = mat

        sparse_threshold = 0.1
        n_non_sparse = (np.abs(mat) > sparse_threshold).sum(axis=(0,1))
        n_non_sparse = n_non_sparse - (np.abs(np.diag(mat)) > sparse_threshold).sum()
        
        # if all(n_non_sparse < min_n_non_sparse):
        #     if all(np.diag(mat) > 0) and mat_has_all_comensalisms(mat):
        #         min_n_non_sparse = n_non_sparse
        #         min_mat = mat

        if mat_has_all_comensalisms(mat):
            print(min_n_non_sparse)
            return mat

    return mat

def gen_simulation_params(K,N):
    max_y = 8000
    for i in range(1000):
        phi = np.array([find_test_matrix(K,0.1,10000)])

        if phi is None:
            continue

        if K == 6: 
            mu = np.array([6.3,5.7,5.3,4.6,4.0,3.0])
        else:
            mu = np.random.normal(size=K)

        y0 = [mu - 1]


        forecast_len = 40

        link = lambda l: np.array(np.rint(np.exp(l)),dtype=int)
#        link = lambda l: random.poisson(np.exp(l))
#        link = lambda l: l
        sigma = np.matrix([random.normal(0,0.01,K) for _ in range(K)])
        sigma = sigma + np.diag([0.2 for _ in range(K)])
        sigma = sigma * sigma.T

        system = evolve_var_system(mu, phi, sigma, y0, N, forecast_len,link=link,nested_alpha=True,random=random)

        if system[1].y.max() <= max_y:
            return((system,phi,mu,sigma))


if __name__ == "__main__":
    from pyRemembeR import Remember
    remember = Remember()
    # system, phi, mu, sigma = gen_simulation_params(K=6, N = 320)
    # remember(phi,"simulated_phi")
    # remember(mu,"simulated_mu")
    # remember(sigma,"simulated_sigma")


    # basic_df = system[1]
    # basic_df.to_feather('data/simulation10.feather')
    # stan_data = {}
    # stan_data['y'] = system[0].T
    # stan_data['N'] = system[0].T.shape[1]
    # stan_data['m'] = system[0].T.shape[0]
    # stan_data['forecast_len'] = 40
    # stan_data['n_seas'] = 0
    # stan_data['seasonality_idx'] = np.array([0])
    # stan_data['n_seas_levels'] = np.array([0])
    # stan_data['season'] = np.array([[0]])

    #     ## add priors
    # stan_data = {**stan_data,
    #              **{'p':1,
    #                 'm_diag':np.array(np.repeat(0,stan_data['y'].shape[0]),dtype=int),
    #                 'scale_diag':1,
    #                 'scale_offdiag':0,
    #                 'es' : [0,0], # top-level prior for the means of the means (diag, off-diag)
    #                 'fs' : [np.sqrt(3), np.sqrt(3)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
    #                 'gs' : [2.1,2.1], # top-level prior for the position of the scales,
    #                 'hs' : [1/3,1/3], # top-level prior for the precision of the scales,
    #                 'df':stan_data['m']+3,
    #                 'alpha':0,
    #                 'sd0': 7, # hyper prior precision of mu0
    #                 'g0':4, # hyper prior for variance of m0
    #                 'h0':3 # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
    #              }
    # }
    
    # pickle.dump(stan_data,open('data/simulation_stan_data.pickle','wb'))


    system, phi, mu, sigma = gen_simulation_params(K=15,N=500)
    remember(phi,"large_simulated_phi")
    remember(mu,"large_simulated_mu")
    remember(sigma,"large_simulated_sigma")


    basic_df = system[1]
    basic_df.to_feather('data/large_simulation.feather')
    stan_data = {}
    stan_data['y'] = system[0].T
    stan_data['N'] = system[0].T.shape[1]
    stan_data['m'] = system[0].T.shape[0]
    stan_data['forecast_len'] = 40
    stan_data['n_seas'] = 0
    stan_data['seasonality_idx'] = np.array([0])
    stan_data['n_seas_levels'] = np.array([0])
    stan_data['season'] = np.array([[0]])

        ## add priors
    stan_data = {**stan_data,
                 **{'p':1,
                    'm_diag':np.array(np.repeat(0,stan_data['y'].shape[0]),dtype=int),
                    'scale_diag':1,
                    'scale_offdiag':0,
                    'es' : [0,0], # top-level prior for the means of the means (diag, off-diag                    'fs' : [np.sqrt(3), np.sqrt(3)], # top-level prior for the precision of means. a pretty tight prior seems to help avoid multimodality
                    'gs' : [2.1,2.1], # top-level prior for the position of the scales,
                    'hs' : [1/3,1/3], # top-level prior for the precision of the scales,
                    'df':stan_data['m']+3,
                    'alpha':0,
                    'sd0': 7, # hyper prior precision of mu0
                    'g0':4, # hyper prior for variance of m0
                    'h0':3 # hyper prior for variance of m0} # degrees of freedom in the inverse wishart prior on the scale matrix.
                 }
    }
    
    pickle.dump(stan_data,open('data/large_simulation_stan_data.pickle','wb'))
