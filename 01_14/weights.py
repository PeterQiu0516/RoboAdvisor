import pandas as pd
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns

def weights(data):
    #df = pd.read_csv("complete_new.csv",parse_dates=[0], index_col=0,infer_datetime_format=True)
    df = data
    fund = df.iloc[0:,0:5]

    mu = expected_returns.mean_historical_return(fund)
    S = risk_models.sample_cov(fund)
    
    ### Method 1: Markowitz Mean-Variance Model
    ef = EfficientFrontier(mu, S,weight_bounds=(0.05,0.4))
    raw_weights = ef.max_sharpe()
    cleaned_weights = ef.clean_weights()
    #print(cleaned_weights)
    #ef.save_weights_to_file("weights.csv")  # saves to file
    #ef.portfolio_performance(verbose=True)
    
    weights = pd.DataFrame(cleaned_weights.values(),index =cleaned_weights.keys(),columns=["weights"])
    weights_T = pd.DataFrame(weights.values.T,index= weights.columns, columns = weights.index)
    #print(weights_T)
    
    ### Method 2: Black-litterman Model
    
    
    
    ### Output
    df = df.append(weights_T,sort=False)
    #print(df[-10:])
    
    
    data_index = pd.DataFrame(df.index,index = df.index)
    return_data = pd.concat([data_index,df],axis = 1)
    #print(return_data)
    return return_data

data = pd.read_csv("complete_new.csv",parse_dates=[0], index_col=0,infer_datetime_format=True)
print(type(data.index[0]))
data = weights(data)
#print(data[-5:])