import requests
import pickle
import logging

def preprocess_data(data):
    return list(map(int, data.split())) 

class ProblemLoader():
  def __init__(self, url, fname="100kint.p", preprocessor=preprocess_data):
    self.url = url
    self.preprocess_data = preprocessor
    self.fname = fname

  def fetch(self, clear=False):
    values = []

    if clear:
        logging.debug("Ignoring any existing dataset!")
        logging.info("Preprocess new dataset")
        r = requests.get(self.url, allow_redirects=True)
        values = self.preprocess_data(r.content)
        pickle.dump(values, open(self.fname, "wb"))
        return values
        
    try:
        logging.info("Loading existing dataset")
        values = pickle.load(open(self.fname, "rb"))
    except:
        logging.debug("Failed to load existing dataset!")
        logging.info("Preprocess new dataset")
        r = requests.get(self.url, allow_redirects=True)
        values = self.preprocess_data(r.content)
        pickle.dump(values, open(self.fname, "wb"))
    return values
