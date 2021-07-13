import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
from psaw import PushshiftAPI
import matplotlib.pyplot as plt
import re

# Global variables


MAIN_PATH = '/Volumes/Survey_Social_Media_Compare/Methods/'

JOB_QUERY =  '(jobs | employment)'#'(jobs OR employement) -steve -blow'

VACC_QUERY = '(vacc | vax | vaccine | vaccination) (corona | covid) #'(vacc OR vax OR vaccine OR vaccination) (corona OR covid)'

POST_KEEP_FIELDS = [
	'id', 
	'created_utc', 
	'retrieved_on',
	'score', 
	'subreddit', 
	'subreddit_id',
	'num_comments',
	'num_crossposts',
	'upvote_ratio',
	'full_link'
	'title',
	'selftext']


COMMENTS_KEEP_FIELDS = [
	'id',
	'link_id',
	'parent_id',
	'created_utc',
	'retrieved_on',
	'score',
	'subreddit',
	'subreddit_id',
	'permalink',
	'body']	




# Class
class RedditData():

	main_path = MAIN_PATH

	def __init__(self, topic):

		# Initialize all the stuff
		self.topic = topic
		self.result = list()
		self.all_data = dict()
		self.logs = dict()
		self.t_total = 0

		# Date ranges (mondays)
		self.drange = self.survey_dates()

		# Determine search terms based on topic
		if topic == 'Employment':
			self.search_terms = JOB_QUERY

		elif topic == 'Vaccination':
			self.search_terms = VACC_QUERY


	def build_query():
		pass

	def get_date():
		pass

	@classmethod
	def survey_dates():
		pass

	@classmethod
	def to_utc_timestamp(cls, date_ts):
		dateDT = date_ts.replace(tzinfo = timezone.utc).timestamp()
    
    	return dateDT

	@classmethod
	def week_from_day(cls,day):
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
		'''Returns DatetimeIndex (UTC) first to
		last monday(+1) in the survey data.
		
		To get string:
			drange[n].strftime('%Y-%m-%d') 

		To get timestamp(int):
			drange[n].timestamp()
		'''

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
		drange = pd.date_range(
						first_date, 
						last_date, 
						freq = 'W-MON',
						tz = 'utc'
						)

		return drange



class RedditPosts(RedditData):

	def __init__(self):
		super().__init__()
		self.keep_fields = POST_KEEP_FIELDS


	def build_query(
		self,
		start_date,
		end_date
		):

	self.api_request_generator = api.search_submissions(q=self.search_terms,
                                              after = start_time,
                                              before = end_time,
                                              subreddit_subscribers = '>2',
                                              limit = 1000,
                                              over_18 = False,
                                              is_blank = True,
                                              author = '![deleted]',
                                              filter = self.keep_fields)

	def get_data(self):

		self.df = pd.DataFrame([submission.d_ for submission in self.api_request_generator])
		






class RedditComments(RedditData):

	def __init__(self):
		super().__init__()


