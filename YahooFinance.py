import yfinance as YahooFinance
import datetime
import sys
import pandas as pd

ticker = sys.argv[1]
start_dt = sys.argv[2].split('-')
end_dt = sys.argv[3].split('-')


CompanyInfo = YahooFinance.Ticker(ticker)
startDate = datetime.datetime(int(start_dt[0]), int(start_dt[1]), int(start_dt[2]))
endDate = datetime.datetime(int(end_dt[0]), int(end_dt[1]), int(end_dt[2]))

'''
for key, value in AmazonInfo.info.items():

    print(key, ":", value)
'''
stock_price_df = CompanyInfo.history(start=startDate,end=endDate)
output_filename = ticker+'.csv'
stock_price_df.to_csv(output_filename, encoding='utf-8')