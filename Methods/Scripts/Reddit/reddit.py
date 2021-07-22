import pandas as pd
import numpy as np
from datetime import datetime, timedelta, timezone
from psaw import PushshiftAPI
import matplotlib.pyplot as plt
import re
from os import path
import time
import json

# Global variables


MAIN_PATH = '/Volumes/Survey_Social_Media_Compare/Methods/'


job_terms = [
	'jobs',
	'employment'
]


vacc_terms1 = [
	'vacc',
	'vax',
	'vaccine',
	'vacine',
	'immunization',
	'immunisation',
	'vaccines'
]

vacc_terms2 = [
	'corona',
	'covid',
	'covid-19',
	'covid19',
	'pandemic'
]

JOB_QUERY = '({})'.format(
	" | ".join(job_terms)
	)

VACC_QUERY = '({})+({})'.format(
	"|".join(vacc_terms1),
	"|".join(vacc_terms2)
	)

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
	'full_link',
	'title',
	'selftext',
	'is_created_from_ads_ui',
	'is_robot_indexable',
	'is_reddit_media_domain',
	'is_blank']


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
		start_date = int(self.drange[week_num].timestamp())
		end_date = int(self.following_monday(self.drange[week_num]).timestamp())

		# Get current week as str
		self.current_week = self.drange[week_num].strftime('%Y-%m-%d')

		# Build query
		self.build_query(start_date, end_date)

		# Get the data
		self.all_data[self.current_week] = self.get_data()


		t_end = time.time()

		t_week = round(t_end-t_start, 2)

		self.t_total += t_week

		# Calculate the time covered.
		most_recent = max(self.all_data[self.current_week]['date'])
		oldest = min(self.all_data[self.current_week]['date'])

		time_covered = str(most_recent - oldest)

		self.logs[self.current_week] = {
			'weekNum': week_num,
			'weekStart': self.current_week,
			'weekEnd': self.following_monday(self.drange[week_num]).strftime('%Y-%m-%d'),
			'mostRecent': str(most_recent),
			'oldest': str(oldest),
			'periodCovered': time_covered,
			'total': self.total_results,
			'totalOverall': self.total_results_overall,
			'timeTaken': t_week,
			'timeTakenTotal': self.t_total
		}

	def export_one_week(self):
		df_path = path.join(
						self.data_path,
						'CSV/{}.csv'.\
						format(self.current_week)
						)

		self.all_data[self.current_week].to_csv(df_path)

		logs_path = path.join(
						 self.data_path,
						 'LOGS/{}.json'.\
						 format(self.current_week)
						 )

		with open(logs_path, 'w') as fout:
			json.dump(self.logs[self.current_week], fout)


	@classmethod
	def load_df(cls,path):
		df = pd.read_csv(path, index_col=0)
		return df

	@classmethod
	def load_log(cls, path):
		with open(path) as fin:
			log = json.load(fin)
		return log

	@classmethod
	def load_log(cls,path, df = False):
		with open(path) as fin:
			log = json.load(fin)

		if df == True:
			log_df = pd.json_normalize(log)
			return log_df
		else:
			return log

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

	@classmethod
	def get_ids(cls, df):
		ids = df[df['num_comments'] > 2].get('id')

		return list(ids)

	@classmethod
	def id_chunks(cls, ids, n):
		"""Yield successive n-sized chunks from lst."""
		for i in range(0, len(ids), n):
			yield ids[i:i + n]

	@classmethod
	def load_one_week(
		cls,
		topic,
		week_num,
		post_type,
		df_only = True
		):

		df, log = None, None

		if post_type == 'Submissions':
			obj = RedditSubmissions(topic)

		elif post_type == 'Comments':
			obj = RedditComments(topic)

		elif post_type == 'SubComments':
			obj = RedditSubComments(topic)

		week_start_date = obj.drange[week_num].strftime('%Y-%m-%d')

		df_path = path.join(
					   obj.data_path,
					   'CSV/{}.csv'.\
					   format(week_start_date))

		df = cls.load_df(df_path)

		if df_only == True:
			return df
		else:
			log_path = path.join(
							 obj.data_path,
							 'LOGS/{}.json'.\
							 format(week_start_date))

			log = cls.load_log(log_path)

			return df, log


	@classmethod
	def load_all(
		cls,
		topic,
		post_type,
		start_week = 0,
		end_week = 1,
		df_only = True):

		df_list, all_logs = list(), dict()
		drange = cls.survey_dates()

		for week_num in range(start_week, end_week + 1):

			cw_date = str(drange[week_num]).\
				split(' ')[0]

			df, log = cls.load_one_week(
				topic = topic,
				week_num = week_num,
				post_type = post_type,
				df_only = df_only)

			df_list.append(df)
			all_logs[cw_date] = log

		all_dfs = pd.concat(df_list)

		return all_dfs, all_logs

class RedditSubmissions(RedditData):

	def __init__(self,topic):
		super().__init__(topic)
		self.keep_fields = SUBMISSIONS_KEEP_FIELDS
		self.api = PushshiftAPI()
		self.data_path = path.join(
								self.main_path,
								'Data/Reddit/Raw/{}/Submissions'.\
								format(self.topic))


	def build_query(
		self,
		start_date,
		end_date
		):

		self.api_request_generator = self.api.search_submissions(
											q=self.search_terms,
											after=start_date,
											before=end_date,
											subreddit_subscribers='>2',
											over_18=False,
											author='![deleted]',
											filter=self.keep_fields
											)

class RedditComments(RedditData):

	def __init__(self, topic):
		super().__init__(topic)
		self.keep_fields = COMMENTS_KEEP_FIELDS
		self.api = PushshiftAPI()
		self.search_ids = search_ids
		self.sub_ids = ids

		
		self.data_path = path.join(
								self.main_path,
								'Data/Reddit/Raw/{}/Comments/Queried'.\
								format(self.topic))

	def build_query(
		self,
		start_date,
		end_date
		):

		# Request based on [topic]_QUERY
		self.api_request_generator = self.api.search_comments(
											q=self.search_terms,
											after = start_date,
											before = end_date,
											author = '![deleted],!AutoModerator',
											filter = self.keep_fields
											)

class RedditSubComments(RedditData):

	def __init__(self, topic, sub_ids, chunk_size):
		super().__init__(topic)
		self.keep_fields = COMMENTS_KEEP_FIELDS
		self.api = PushshiftAPI()
		self.sub_ids = sub_ids
		self.chunk_size = chunk_size

		self.data_path = path.join(
								self.main_path,
								'Data/Reddit/Raw/{}/Comments/SubID'.\
								format(self.topic))

	def build_query(
		self,
		start_date,
		end_date
		):

		self.api_request_generator = self.api.search_comments(
										link_id=self.sub_ids,
										after=start_date,
										before=end_date,
										author = '![deleted],!AutoModerator',
										filter = self.keep_fields
										)




	def get_sub_comments(self, week_num):

		self.current_week = self.drange[week_num].strftime('%Y-%m-%d')

		df_list = []

		id_subsets = RedditData.id_chunks(self.sub_ids, self.chunk_size)
		n_chunk = 0

		for id_subset in id_subsets:

			current_sub = RedditSubComments(
								topic = self.topic, 
								sub_ids = id_subset,
								chunk_size = self.chunk_size)

			current_sub.get_one_week(week_num)

			df_list.append(current_sub.all_data[current_sub.current_week])

			if n_chunk == 0:
				all_logs = current_sub.logs[current_sub.current_week]

			else:
				all_logs.update({

					'mostRecent': max(all_logs['mostRecent'],
						current_sub.logs[self.current_week]['mostRecent']),
					'oldest': min(all_logs['oldest'],
						current_sub.logs[self.current_week]['oldest']),
					'periodCovered': max(all_logs['periodCovered'],
						current_sub.logs[self.current_week]['periodCovered']),
					'total': all_logs['total'] + \
						current_sub.logs[self.current_week]['total'],
					'totalOverall': all_logs['totalOverall'] + \
						current_sub.logs[self.current_week]['totalOverall'],
					'timeTaken': all_logs['timeTaken'] + \
						current_sub.logs[self.current_week]['timeTaken'],
					'timeTakenTotal': all_logs['timeTakenTotal'] + \
						current_sub.logs[self.current_week]['timeTakenTotal']
				})
				
			n_chunk += 1

		self.all_data[self.current_week] = pd.concat(df_list)
		self.logs[self.current_week] = all_logs








