



## 0  Data Loading & Data QC
		
	# 0.1  load data sources and packages into R

		cost = read.csv("InterviewData_Cost.csv")
		rev = read.csv("InterviewData_Rev.csv")
		activity_data = read.csv("InterviewData_Activity.csv")
		parsing = read.csv("InterviewData_Parsing.csv")

		library(dplyr)
		library(tidyr)

	# 0.2  Compare record counts to input files

		dim(cost)
		#10000     3

		dim(rev)
		#10000     3

		##  Control totals match csv by manual inspection.

	# 0.3  Observe column headers

		names(cost)
		#"date"      "source_id" "cost" 

		names(rev)
		#"date"      "source_id" "revenue"

	# 0.4  Test population of attributes

		nrow(rev[is.na( rev[1:2] ),])
		#0

		nrow(rev[is.na( cost[1:2] ),])
		#0

	# 0.5  Test cardinality

		anyDuplicated(cost[1:2])
		#0

		anyDuplicated(rev[1:2])
		#0

		##  Hence the join criteria are 1:1 if a relationship exists.

	# 0.6  Test join fitness with venn-diagram logic

		nrow(setdiff( cost[1:2] , rev[1:2] ))
		#4618 = unique rev attributes

		nrow(setdiff( rev[1:2] , cost[1:2] ))
		#4618 = unique cost attributes

		nrow(intersect( rev[1:2] , cost[1:2] ))
		#5382 = common attributes

		nrow(union( rev[1:2] , cost[1:2] ))
		#14618 = Total distinct attributes	

		5382 + 4618 + 4618
		#14618 = disjoint subsets equal union

		10000 + 10000 - 5832
		#14618 = input data sets less common attributes equal union

		5382/14618
		# ~37% match rate

		##  The data are notably disjoint. Consider whether joins are well populated enough for the intended output.

	# 0.7  Test population of cost and revenue values

		x = rev[is.na(rev[3]),]
		y = cost[is.na(cost[3]),]

		nrow(setdiff( y[1:2] , x[1:2] ))
		#100 = unique rev attributes with unpopulated values

		nrow(setdiff( x[1:2] , y[1:2] ))
		#79 = unique cost attributes with unpopulated values

		nrow(intersect( x[1:2] , y[1:2] ))
		#0 = common attributes with unpopulated values

		nrow(union( x[1:2] , y[1:2] ))
		#179 = Total distinct attributes with unpopulated values

		179 / 14618
		# ~1% unpopulated

		##  By manual inspection the missing values are dispersed rougly evenly among date and source_id attributes. 
		##  Do we understand why the data is partial? How do we address the gaps?

## 1 ## Question 1

	# 1.1  Objective:

		#Using base R functions (i.e., without using functions/packages beyond those in the default installation),
		#join these two data sets by “date” and “source_id”, returning all rows from both regardless of whether
		#there is a match between the two data sets.

	# 1.2  Solution: base R merge() 

		q1 <- merge( 
			 x = cost
			,y = rev
			,by.x = c("date","source_id")
			,by.y = c("date","source_id")
			,all.x = TRUE
			,all.y = TRUE
			)

		# QC : Do q1 attributes match the union of attributes?
		setequal(q1[1:2],union(rev[1:2],cost[1:2]))
		#TRUE

		#print(q1)

## 2  Question 2

	# 2.1  Objective:

		#Using any functions/packages you want in R, join these two data sets by “date” and “source_id”,
		#returning only the rows from the “Cost” file that have no corresponding date in the “Revenue” file.

	# 2.2  Solution: dplyr anti_join()

		q2 <- anti_join (cost, rev)
		#Joining, by = c("date", "source_id")

		# QC : Does merge() output match anti_join() output?
		q2t <- merge( 
			 x = cost
			,y = setdiff(cost[1:2],rev[1:2])
			,by.x = c("date","source_id")
			,by.y = c("date","source_id")
			,all.x = FALSE
			,all.y = FALSE
			)

		setequal(q2,q2t)
		#TRUE

		#print(q2)

## 3  Question 3

	# 3.1  Objective A:

		#Using your result from #1, what are the Top 4 sources (“source_id” values) in terms of total revenue
		#generation across this data set? 
		
	# 3.2  Solution to A: dlpyr pipe 

		q3 <- q1 %>% 
			group_by(source_id) %>% 
			summarise(revenue = sum(revenue, na.rm = TRUE)) %>% 
			arrange(-revenue)

		# QC : does the pipe output match aggregate() output?
		setequal(q3,aggregate(revenue~source_id,q1,sum))
		#TRUE

		head(q3[1],4)

		##  Top 4 source_id by revenue:
		# 1    PA0527
		# 2    PA0308
		# 3    PA0352
		# 4    PA0552	

	# 3.3  Objective B: 

		#How would you visualize the monthly revenue for those Top 4 sources?
		#(note: you don’t need to actually create a plot; you can just describe what your ideal visual would look
		#like)

	# 3.4 Solution to B:

		# I would create a time series chart with time along the x-axis and revenue metrics along the y-axis.
		# The chart would contain overlaid plots of each top 4 source and comparison overlays of the average source in y-axis quartile.
		# This would allow stakeholders to visualize relative purchasing peaks, month-to-month velocity, and compare top sources to lower source groups.
		
		# I can envision various useful x/y views:
			# x-axis = chronological month and year. 
			# x-axis = months only. Include customer by year as separate overlaid plots to evaluate effects of periodic marketing campaigns or seasonality.
			# x-axis = distance in months from a date of business interest.

			# y-axis = average revenue per source. 
			# y-axis = average monthly revenue by source as a % of total monthly revenue for all sources.
			# y-axis = number of records per month or average revenue per record (if a record is 1:1 with an economic event, such as a CC payment).
			# y-axis = profit margin % based on the joined cost & revenue view data from Question 1.

		# to publish my visual, I would:
			# Reduce white space and preserve scale with a close to minimum, non-zero y-intercept.
			# Include useful headings, labels, and a legend to help the document describe itself so anyone can pick it up and learn.
			# Identify patterns that should influence our market approach and include visual callouts in the charts to help clarify key findings.
			# Size the materiality, such as with a pie chart highlighting the top 4 customers's y-axis value as % of the total data set.
			# Consider visual methods of forecasting future results based on visual trend analysis.

## 4  Question 4

	# 4.1  Objective:

		# Assuming you’ve read the data into an R object called activity_data, run the following code to build a
		# basic logistic regression model. 

		full_logit_model <- glm(formula = active ~ age + gender + metropolitan_area +
		device_type,
		data = activity_data,
		family = binomial(link = "logit"))

		#Apply this model to the same data that the model was trained on and assess the prediction accuracy.

	# 4.2  Solution : using base R predict() and arbitrary decision boundary

		# Apply model to its training data; type=response gives the vector of P( active = 1 | full_logit_model ) for each observation in activity data.
		q4p <- predict(
			object=full_logit_model
			,newdata=activity_data
			,type='response'
			)

		# Define a simple classifier wherein we expect active = 1 if and only if P > 50% 
		q4c <- ifelse(q4p >.5,1,0) 

		# Consider a simple accuracy measure as the % of correct classifications
		1-mean( q4c != activity_data$active)
		# 0.5806273 = % of correct classifications

# 5  Question 5

	# 5.1 Objective:

		# Split the data into training and test samples, and build a model over the training data using the following R code:

		training_data <- activity_data[1:4000,]
		test_data <- activity_data[4001:5420,]
		training_logit_model <- glm(formula = active ~ age + gender + metropolitan_area +
		device_type,
		data = training_data,
		family = binomial(link = "logit"))

		# Assess the training data model’s accuracy on the test data. Why does the accuracy change so much?

	# 5.2 Solution: partititoning on activity_data

		# Using the same methodology as Question 4 for comparability:
		q5p <- predict(
			object=training_logit_model
			,newdata=test_data
			,type='response'
			)

		q5c <- ifelse(q5p >.5,1,0)

		1-mean( q5c != test_data$active)
		# 0.2112676= % of correct classifications

		# Suppose we wanted to infer the active flag for a hypothetical source with certain attributes.
		# If we use all the data we have to deduce the best rules, as in Question 4, then we cannot test our logic on independent observations.
		# This introduces propensity for upward bias (aka optimism) in the accuracy metrics because the logit regression is designed to optimize best fit for the given observations.
		# We can measure the impact of this propensity by separating the data into disjoint training and test subsets, as in Question 5, and comparing accuracies.
		# Due to the effect of optimism, we can always expect cross-validated accuracy to be less than or equal to non-validated accuracy.

# 6  Question 6

	# 6.1 Objective:

		# This data comes from a subset of userdata JSON blobs stored in our database. Parse out the values
		# (stored in the “data_to_parse” column) into four separate columns. So for example, the four additional
		# columns for the first entry would have values of “N”, “U”, “A7”, and “W”. You can use any R
		# functions/packages you want for this.

	# 6.2 Solution: tidyr separate()

		q6 <- parsing %>% 
			separate(data_to_parse,c('a','b','c','d','e'),sep = '"',remove=FALSE) %>%
			select(userid,data_to_parse,d) %>%
			separate(d,c('value1','value2','value3','value4'),sep = ';') 

		#print(q6)

# 7 Additional Question B

	# 7.1 Objective:

		# Within our web and mobile apps, members can generally find items through search and/or the product
		# category tree (note that you can also search after clicking into a product category, in which case the
		# search is filtered by the chosen category). Let's say that we decide to test a different product category
		# tree. The Product team asks for your help in setting up the test and calling the results. How would you
		# help them: (i) figure out how long we should run this test; (ii) decide what metric to measure; (iii) and
		# then evaluate the test?

	# 7.2 Solution:

		# How long to run the test depends on the underlying variability among observations of the metric we want to measure, 
		# and the metric type further depends on the purpose of the test. The product tree is designed to help members identify 
		# products they want to buy. How can we tell if our categorization method is helpful? There are many ways, for example, 
		# purchasing behavior can be tracked in data as the number of app visits and the number of conversions over time. Then we 
		# can measure how likely a user is to buy under a given product tree by evaluating the ratio of visits to conversions. We 
		# can further measure the likelihood that a variant in the tree improved over a prior version by comparing conversion ratios 
		# between trees. 

		# Then, how long do we test a proposed variant? It depends on how many observations we experience over time. Based on historical 
		# app visit data we can forecast the amount of time it takes to reach a given number of visits. If the test involves multiple 
		# simultaneous variants, traffic can be allocated at will between to achieve desired sample sizes. 

		# How do we know how many observations we should collect?  Under certain assumptions about the data, we can measure the number 
		# of observations needed with statistical Confidence intervals. To do this, we consider our sensitivity to:

		# (a) prior conversion rates – how do we know which variant we should be comparing with?

		# (b) how much of a ratio improvement we want to detect (the Minimum Detectable Effect). Large improvements are observable with 
		# fewer observations, and vice versa (all else equal).

		# (c) how willing we are be incorrect (the Statistical Significance).  

		# As a practical matter, even if we are not sure about (a), (b), or (c) we can parameter-ize them and consider scenarios of different 
		# sample sizes needed to measure different sensitivity levels. This allows us to compare various assumptions and choose a solution that 
		# makes sense in context.
