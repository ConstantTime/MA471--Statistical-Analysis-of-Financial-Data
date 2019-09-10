#!/usr/bin/env python
# coding: utf-8

# In[3]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as stats

data = pd.read_csv('data.txt', sep="\s+")


# In[4]:


plt.scatter(data['SP'], stats.norm.sf(data['SP']), label="Normal", marker='.');
plt.scatter(data['SP'], stats.t.sf(data['SP'], df=10), label="T", marker='.');
plt.scatter(data['SP'], stats.laplace.sf(data['SP']), label="Double Exponential", marker='.');
plt.scatter(data['SP'], stats.cauchy.sf(data['SP']), label="Cauchy", marker='.');
plt.legend();
plt.title('Survival Plots')
plt.plot();


# In[5]:


plt.scatter(data['C'], stats.norm.sf(data['C']), label="Normal", marker='.');
plt.scatter(data['C'], stats.t.sf(data['C'], df=10), label="T", marker='.');
plt.scatter(data['C'], stats.laplace.sf(data['C']), label="Double Exponential", marker='.');
plt.scatter(data['C'], stats.cauchy.sf(data['C']), label="Cauchy", marker='.');
plt.legend();
plt.title('Survival Plots')
plt.plot();


# In[ ]:




