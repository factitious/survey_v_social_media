# Imports
from datetime import date, datetime, timedelta
import time
from os import path
from searchtweets import load_credentials
from searchtweets import ResultStream, gen_request_parameters 
import pandas as pd
import numpy as np
import json
import openpyxl
import time



MAIN_PATH = '/Volumes/Survey_Social_Media_Compare/Methods/'

JOB_QUERY = '(jobs OR employement) -steve -blow'

VACC_QUERY = '(vacc OR vax OR vaccine OR vaccination) (corona OR covid)'


class TwitterData():
	'''Query Twitter API and save data.'''

	main_path = MAIN_PATH

	def __init__(self, topic):

		# Initialize all the stuff
		self.topic = topic
		self.credentials = dict()
		self.all_requests = 0
		self.total_results_overall = 0
		self.query = ''
		self.rs = None
		self.result = list()
		self.all_data = dict()
		self.logs = dict()
		self.t_total = 0

		# Date ranges (mondays)
		self.drange, \
		self.drange_str = \
			self.survey_dates()

		# Determine search terms based on topic
		if topic == 'Employment':
			self.search_terms = JOB_QUERY

		elif topic == 'Vaccination':
			self.search_terms = VACC_QUERY



	def validate_credentials(self):
		'''Load API credentials from twitter_keys.yaml.'''

		c_path = path.join(
			self.main_path, 
			'Scripts/Twitter/twitter_keys.yaml'
			)

		self.credentials = load_credentials(
								c_path,
								env_overwrite = True
								)

		return "Credentials validated successfully!"

	def build_query(
		self,
		start_date,
		end_date,
		inQuotes = False,
		language = 'en',
		country = 'US',
		exclude_rt = True,
		results_per_call = 500,
		return_fields = 'id,created_at,text,public_metrics, retweet_count',
		otherTerms = []
		):

		'''
		Builds the query that is used to make the requests and get payloads.

		Parameters:
			mainTerms (str): The search terms we want, e.g. 'jobs'
			start_date (str): The lower end of the period we are interested in.
			 				 YYY-MM-DD HH:MM format, e.g. '2020-10-23 13:00'
			end_date (str): The higher end of the period we are interested in 
							 YYY-MM-DD HH:MM format, e.g. '2020-10-23 14:00'
			inQuotes (bool): If true the terms will be put in quotes
			language (str): Language used in the query.
							Only languages supported by Twitter + 
							has to be in the correct format
							see https://bit.ly/2RBwmGa
			country (str): Country where Tweet/User is located.
						   Has to be in the correct format, see
						   https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)
			exclude_rt (bool): Exclude retweets from the payload? 
			results_per_call (int): How many results per request? 
							  Max is 500 for the academic API.
			otherTerms (list): List of other search terms, e.g. ['#COVID', 'is:reply']

		Notes:
			- More notes on building queries here: 
			  twitter-api/tweets/search/integrate/build-a-query.
			- Tweets are fetched in reverse chronological order, 
			   i.e. starting at endDate.
			- endDate refers to previous day until 23:59
		'''

		# If excluding retweets, set rt to '-' 
		rt = '-is:retweet' if exclude_rt == True else ''

		# Are the terms in quotes? (i.e. exact match)
		self.search_terms = '"{}"'.format(self.search_terms) \
							if inQuotes == True \
							else '{}'.format(self.search_terms)

		#  Build qury text
		query_text = '{} lang: {} place_country:{}'.\
							format(self.search_terms,
								   language,
								   country)

		# If there are other terms, include them in the query_text
		query_text = query_text.extend(other) if otherTerms != [] else query_text
		
		# Save these as will be used to determine limits
		self.results_per_call = results_per_call
		
			
		# Build query
		self.query = gen_request_parameters(query_text,
									  start_time = start_date,
									  end_time = end_date,
									  tweet_fields = return_fields,
									  results_per_call = self.results_per_call)


	def get_data(self,nTweets):
		'''Use ResultStream to get the data'''

		self.rs = ResultStream(request_parameters = self.query,
						  max_tweets = nTweets,
						  output_format = "a",
						  **self.credentials)

		self.result = list(self.rs.stream())
		
		# We can get the total requests made for a payload using:
		# twitterData_instance.rs.n_requests
		# twitterData_instance.rs.session_request_counter
		
		# This can be used to get the overall requests made and saving logs.
		self.all_requests += self.rs.session_request_counter	   
		self.total_results_overall += self.rs.total_results


	def get_one_week(self, week_num):

		t_start = time.time()

		# Get start and end date from the week number.
		start_date = self.drange_str[week_num-1][0]
		end_date = self.drange_str[week_num-1][1]



		# Build query.
		self.build_query(start_date, end_date)

		# Get the data.
		self.get_data(nTweets = 200000)

		# Get dataframe
		self.all_data[start_date] = TwitterData.get_df(self.result)

		t_end = time.time()

		t_week = round(t_end - t_start,2)

		self.t_total += t_week

		# Calculate the time covered in a payload.
		# Most recent date/time in the df in datetime format
		most_recent = self.to_datetime(max(self.all_data[start_date]['created_at']))
		oldest = self.to_datetime(min(self.all_data[start_date]['created_at']))

		time_covered = str(most_recent - oldest)

		self.logs[start_date] = {
			'mostRecent': str(most_recent),
			'oldest': str(oldest),
			'periodCovered': time_covered,
			'sessionRequestCounter': self.rs.session_request_counter,
			'totalRequests': self.all_requests,
			'totalTweets': self.rs.total_results,
			'totalTweetsOverall': self.total_results_overall,
			'requestParams': self.rs.request_parameters,
			'timeTaken': t_week,
			'totalTimeTaken': self.t_total
			}

        # Save current week's Monday date.
        # Will be used to name the files when saving.
		self.current_week = start_date

	@staticmethod
	def get_df(result):
		'''
		'''
		# Remove the entries (i.e. dictionaries) that contain
		# the key 'newest_id' from the payload, i.e. the result 
		# of our query (which is a list of dictionaries).		
		clean_json_list = [x for x in result if 'newest_id' not in x]		
		
		# Get date in nice pd.df 
		df = pd.json_normalize(clean_json_list)

		return df

	def export_one_week(self):

		json_path = path.join(self.main_path, 
							 'Data/Twitter/{}/JSON/{}.json'.\
							 format(self.topic,self.current_week))

		
		df_path = path.join(self.main_path,
							'Data/Twitter/{}/CSV/{}.csv'.\
							format(self.topic, self.current_week))

		logs_path = path.join(self.main_path,
							 'Data/Twitter/{}/LOGS/{}.csv'.\
							 format(self.topic, self.current_week))

			
		with open(json_path, 'w') as fout:
			json.dump(self.result, fout)


		self.all_data[self.current_week].to_csv(df_path)

		with open(logs_path, 'w') as fout:
			json.dump(self.logs, fout)


	@classmethod
	def load_one_week(
		cls,
		topic,
		week_num, 
		loadDF = True, 
		loadJSON = False):
		'''
		week_num (int): name of file to be loaded
		topic (str): "Employment" or "Vaccination"
		
		'''

		obj = TwitterData(topic)
		
		week_start_date = obj.drange_str[week_num-1][0]

		df_path = path.join(
						obj.main_path,
						'Data/Twitter/{}/CSV/{}.csv'.\
						format(topic, week_start_date)
						)

		json_path = path.join(
						obj.main_path,
						'Data/Twitter/{}/JSON/{}.json'.\
						format(topic, week_start_date)
						)


		if(loadDF and not loadJSON):
			df = pd.read_csv(df_path, index_col=0, dtype={'id': object})
			return df
		
		if(not loadDF and loadJSON):
			with open(json_path) as f:
				result = json.load(f)
			
			return result
		
		if(loadDF and loadJSON):
			df = pd.read_csv(df_path, index_col=0, dtype={'id': object})
			
			with open(json_path) as f:
				result = json.load(f)
					
			return df, result			   
	

	@classmethod
	def to_datetime(cls,date_str):
		'''
		Take a date in the ISO format that we get from twitter 
		i.e. "%Y-%m-%dT%H:%M:%S.000Z"
		Transform to a datetime.

		Parameters:
			dateStr (str): A date string (ISO format)
		
		Returns:
			dateDT (datetime): A datetime object  
		'''
		
		try:
			date_dt = datetime.strptime(date_str, "%Y-%m-%d")
			
		except: 
			date_dt = datetime.strptime(date_str, "%Y-%m-%dT%H:%M:%S.000Z")
			
		
		return date_dt

	@classmethod
	def week_from_day(cls,day):
		'''
		Work out the week starting and ending dates given any date.
		Params:
			day (Timestamp/datetime)

		Returns: 
			week_start (Timestamp)
			week_end (Timestamp)
		'''

		week_start = day - timedelta(days=day.weekday())
		week_end = week_start + timedelta(days=6)

		return week_start.strftime('%Y-%m-%d'), week_end.strftime('%Y-%m-%d')

	@classmethod
	def following_monday(cls,date):
		'''Get (date of) following monday based on date'''
		date = cls.to_datetime(date)

		next_m = date + timedelta(days=-date.weekday(), weeks=1)

		return next_m

	@classmethod
	def survey_dates(cls):

		# Path to survey periods file
		s_path = path.join(
					cls.main_path, 
					'Scripts/Surveys/table_details/surveyPeriods.xlsx'
					)

		# Load survey periods
		ai_periods = pd.read_excel(s_path, sheet_name = 'AI+HPS')

		# Get first and last dates from the periods.
		first_date, _ = cls.week_from_day(ai_periods['A_I_start_date'][0])
		_, last_date = cls.week_from_day(ai_periods['A_I_start_date'].iloc[-1])

		# Create date range from first to last date (mondays)
		mondays = pd.date_range(
						first_date, 
						last_date, 
						freq = 'W-MON'
						)

		mondays_str = mondays.strftime('%Y-%m-%d')

		mondays_leading = pd.date_range(
							cls.following_monday(first_date),
							cls.following_monday(last_date)
							)

		mondays_leading_str = mondays_leading.strftime('%Y-%m-%d')

		# Tuple
		drange = [(m, s) for m, s in zip(mondays, mondays_leading)]
		drange_str = [(m, s) for m, s in zip(mondays_str, mondays_leading_str)]


		return drange, drange_str



		






























