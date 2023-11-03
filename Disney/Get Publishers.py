#%%
import requests
from bs4 import BeautifulSoup

def search_and_extract_movie_distributor(movie_list):
    movies = []
    for movie in movie_list:
        # Format the movie title for the Wikipedia search URL
        search_url = f"https://en.wikipedia.org/wiki/{movie}"

        # Send a GET request to the Wikipedia page
        response = requests.get(search_url)

        if response.status_code == 200:
            # Parse the HTML content of the page
            soup = BeautifulSoup(response.content, 'html.parser')

            # Find the table with class 'infobox' which contains movie details
            infobox = soup.find('table', {'class': 'infobox'})

            if infobox:
                # Search for the "Distributed by" row in the infobox
                for row in infobox.find_all('tr'):
                    if "Distributed by" in row.text:
                        # Extract the distributor information
                        distributor = row.find('td')

                        if distributor:
                            # Check if there is an <a> tag inside the distributor
                            distributor_link = distributor.find('a')
                            if distributor_link:
                                distributor_title = distributor_link.text
                            else:
                                distributor_title = distributor.get_text()

                            print(f"{movie}: {distributor_title}")
                            movies.append({movie,distributor_title})
                        else:
                            print(f"{movie}: Information not found")
                            movies.append({movie,'Not Found'})
                        break
                else:
                    print(f"{movie}: Information not found")
                    movies.append({movie,'Not Found'})
            else:
                print(f"{movie}: Information not found")
                movies.append({movie,'Not Found'})

        else:
            print(f"Failed to retrieve data for {movie}")
    return movies

# Example list of movies
import pandas as pd
gpt = pd.read_csv('gptfilms.csv')
#movie_list = ["Avatar_(2009_film)", "Titanic_(1997_film)", "Jurassic_Park_(film)"]

new_movie = search_and_extract_movie_distributor(gpt['Film'])
#%%
pd.DataFrame(new_movie).to_csv('Wiki Get.csv')

