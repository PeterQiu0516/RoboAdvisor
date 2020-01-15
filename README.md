# Roboadvisor
A portfolio selection recommendation system based on Markowitz Mean-Variance Model and Black-Litterman Model implemented on the App "Navigator".
This is my first practical project during my internship at Asset Pro in Beijing. And the project is already implemented on the "Navigator" financial APP with thousands of users.

## Programming Languages

+ Python 3: For model implementation () online data crawling(selenium), project integrating(rpy2) and data processing(pandas).
+ R: For model implementation(PortfolioAnalytics) and online data crawling(rvest).
+ Javascript: For plotting related intuitive financial figures(ECharts) and online survey designing.
+ Html: For online survey designing.

## Building

On Win 10, run

```
pip install RoboAdvisor
```

## Running
Access the '01_15' directory and make sure that the three input files `bloomberg.csv`、`filter.csv` and `newfund.csv` are present in the '01_15' directory.

Then run

```
python final.py
```

And access the following [survey page](localhost:9000)




## Milestones

+  Obtained the historical NAV(Net Asset Value) data for about 800 selected funds from [iFund](https://www.ifund.com.hk/en/companies/) and Bloomberg based on Python, Excel and R.

+  Then I designed a questionaire for investigating the customers' specific needs (like risks and return rates requirements).

+  Then I designed the specific funding recommendation system to satisfy each customer's specific needs based on Python and R. I utilized the famous Markowitz and Black-Litterman model in the main body of the project.

+ ... To be completed.
