import pandas as pd
a=pd.DataFrame({'a':[1,2,3],'c':[2,3,4]})
b=pd.DataFrame({'a':[11,22],'b':[22,33]})
c = pd.concat([a,b])
print(c)