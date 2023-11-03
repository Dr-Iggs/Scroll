#%%
import requests
import pandas as pd
# List of terms to search for in Wikipedia
terms = pd.read_csv('All Movie Data.csv')['Film']

# Loop through the list of terms
for term in terms:
    # Create the Wikipedia URL for the term
    wikipedia_url = "https://en.wikipedia.org/wiki/" + term

    # Send a GET request to the Wikipedia page
    response = requests.get(wikipedia_url)

    # Check if the word "Idaho" appears in the article
    if "Idaho" in response.text:
        print(f"'Idaho' found in the article for {term}.")
# %%
