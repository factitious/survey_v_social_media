import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
from psaw import PushshiftAPI
import matplotlib.pyplot as plt
import re

# Global variables


MAIN_PATH = '/Volumes/Survey_Social_Media_Compare/Methods/'

JOB_QUERY =  '(jobs | employment)'

VACC_QUERY = '(vacc | vax | vaccine | vaccination)&(corona | covid)'

SUBMISSIONS_KEEP_FIELDS = [
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
	'selftext',
	'is_created_from_ads_ui',
	'is_robot_indexable']


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
	'body',
	'locked']	




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
		self.total_results = 0
		self.total_results_overall = 0

		# Date ranges (mondays)
		self.drange = self.survey_dates()

		# Determine search terms based on topic
		if topic == 'Employment':
			self.search_terms = JOB_QUERY

		elif topic == 'Vaccination':
			self.search_terms = VACC_QUERY


	def build_query(self):
		pass

	def get_data(self):

		df = pd.DataFrame([post.d_ for post in self.api_request_generator])

		# Get date
		df['date'] = pd.to_datetime(df['created_utc'], utc = True, unit = 's')


		self.total_results = len(df)
		self.total_results_overall += len(df)
		# The metadata returned by psaw is unreliable.

		return df

	def get_one_week(self, week_num):

		t_start = time.time()

		# Get start and end date from the week number
		start_date = self.drange[week_num].timestamp()
		end_date = self.following_monday(self.drange[week_num]).timestamp()

		# Get current week as str
		self.current_week = start_date.strftime('%Y-%m-%d')

		# Build query
		self.build_query(start_date, end_date)

		# Get the data
		self.all_data[self.current_week] = get_data()


		t_end = time.time()

		t_week = round(t_end-t_start, 2)

		self.t_total += current_week

		# Calculate the time covered.
		most_recent = max(self.all_data[self.current_week]['date'])
		oldest = min(self.all_data[self.current_week]['date'])

		time_covered = str(most_recent - oldest)

		self.logs[self.current_week] = {
			'weekNum': week_num,
			'weekStart': start_date,
			'weekEnd': end_date,
			'mostRecent': str(most_recent),
			'oldest': str(oldest),
			'periodCovered': time_covered,
			'total': self.total_results,
			'totalOverall': self.total_results_overall,
			'timeTaken': t_week,
			'timeTakenTotal': self.t_total
		}

	# @classmethod
	# def to_datetime(cls, date_ts):
	# 	'''Transform utc timestamp (int) to datetime'''

	# 	date_dt = datetime.fromtimestamp(date_ts)

	# 	return date_dt

	# @classmethod
	# def to_utc_timestamp(cls, date_ts):
	# 	dateDT = date_ts.replace(tzinfo = timezone.utc).timestamp()
    
 #    	return dateDT

	@classmethod
	def week_from_day(cls,day):
		week_start = day - timedelta(days=day.weekday())
		week_end = week_start + timedelta(days=6)

		return week_start.strftime('%Y-%m-%d'), week_end.strftime('%Y-%m-%d')

	@classmethod
	def following_monday(cls,date):
		'''Get (date of) following monday based on date'''
		# date = cls.to_datetime(date)

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



class RedditSubmissions(RedditData):

	def __init__(self):
		super().__init__()
		self.keep_fields = SUBMISSIONS_KEEP_FIELDS
		self.api = PushshiftAPI()


	def build_query(
		self,
		start_date,
		end_date
		):

		self.api_request_generator = self.api.search_submissions(
											q=self.search_terms,
											after=start_time,
											before=end_time,
											subreddit_subscribers='>2',
											over_18=False,
											is_blank=True,
											author='![deleted]',
											filter=self.keep_fields
											)




class RedditComments(RedditData):

	def __init__(self, ids = None):
		super().__init__()
		self.keep_fields = COMMENTS_KEEP_FIELDS
		self.api = PushshiftAPI()

		if ids:
			self.ids = ids
		else:
			self.ids = None

	def build_query(
		self,
		start_date,
		end_date
		):

		if self.ids:
			# Get all the comments associated with submissions
			self.api_request_generator = self.api.search_comments(
												link_id=self.sub_ids,
												after=self.start_time,
												before=self.end_time,
												author = '![deleted],!AutoModerator',
												filter = self.keep_fields
												)
		else:
			# Request based on [topic]_QUERY
			self.api_request_generator = self.api.search_comments(
												q=self.search_terms,
												after = start_time,
												before = end_time,
												author = '![deleted],!AutoModerator',
												filter = self.keep_fields
												)

	
	def get_one_week(self, week_num):

		t_start = time.time()

		# Get start and end date from the week number
		start_date = self.drange[week_num].timestamp()
		end_date = self.following_monday(self.drange[week_num]).timestamp()




	def merge_comments_submissions():
		pass









