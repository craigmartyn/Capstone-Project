# Word Prediction Model Documentation

###Background
This model is a tool to predict the next word given a string of text.

###Documentation

####Inputs
The inputs to the model are:

* Text preceding the word that is predicted by the model. The app strips out numbers and punctuation, and is not case sensitive

* Number of words to display: the number of the most liekly next words to be displayed in a word cloud (between 1 and 50)

####Generating the Forecast
To generate the prediction, click the "Update!" button. If you change the text or the number of words to display, click the "Update!" button again to update the predicted next word and word cloud.

####Outputs
The most likely next word is shown, as well as a word cloud of the most likely 15-50 next words, with the numebr shown specified by the user. The size of the words in the word cloud are in proportion to the probability that they are the next word, as estimated in the prediction algorithm.
