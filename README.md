# Movie

This repository is to keep track of all analyses related to movies in general, and the movies I watch.

## Data Setup

Currently I use two main data sources [IMDB](https://datasets.imdbws.com/) which I use for data about all movies that IMDB stores information on and my personal database. My personal database is hosted on DynamoDB and the code for accessing and updating it can be found at [moviedb](github.com/luhann/moviedb).

### Personal Database Setup

My current personal database is relatively simple, I have 3 AWS lambda functions for getting and setting movie information for movies that I have watched. All lambda functions pull information from the [OMDB API](https://www.omdbapi.com/) and use title of the movie and year to get and set information in DynamoDB.

The lambda functions are triggered by AWS API Gateway from a domain hosted on Route53. 

A simplified schematic of the api setup for adding information to Dynamodb looks like this:

Route53 -> API Gateway -> Lambda -> OMDB API Call -> DynamoDB

A simplified schematic of the api setup for getting information from Dynamodb looks like this:

Route53 -> API Gateway -> Lambda -> OMDB API Call -> DynamoDB -> API Gateway

## Repository Structure

### Movie Runtime

There are two versions of this notebook (one in Rmarkdown, and one in a Jupyter notebook), the main goal of this analysis was to answer the questions: __Are movie runtimes getting longer?__ and __Are the runtimes of movies I have personally watched getting longer?__

### Movie Predictor

The main goal of this analysis is to create a movie reccomendation algorithm that is personalised to my tastes.