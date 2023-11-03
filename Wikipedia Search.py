#%%
import requests
from bs4 import BeautifulSoup

# URL of the Wikipedia page
url = "https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Physics"

# Send an HTTP GET request to the URL
response = requests.get(url)

# Check if the request was successful (status code 200)
if response.status_code == 200:
    # Parse the HTML content of the page using BeautifulSoup
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find all the links (anchor tags) on the page
    links = soup.find_all('a')

    # Iterate through the links
    for link in links:
        # Get the href attribute of the link (URL)
        link_url = link.get('href')

        # Check if the link URL is not None and it is a valid Wikipedia link
        if link_url and link_url.startswith('/wiki/'):
            # Construct the full URL by appending the link URL to the base Wikipedia URL
            full_link_url = f"https://en.wikipedia.org{link_url}"

            # Send an HTTP GET request to the linked page
            linked_page_response = requests.get(full_link_url)

            # Check if the request for the linked page was successful
            if linked_page_response.status_code == 200:
                # Parse the content of the linked page
                linked_page_soup = BeautifulSoup(linked_page_response.text, 'html.parser')

                # Check if the word "Idaho" is present in the content
                if "Idaho" in linked_page_soup.get_text():
                    # Print the link URL if "Idaho" is found
                    print(full_link_url)
            else:
                print(f"Failed to retrieve linked page: {full_link_url}")
else:
    print("Failed to retrieve the web page. Status code:", response.status_code)

# %%
