#%%
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re


all = []
# Send an HTTP GET request to the URL
def count_masts(url):
    response = requests.get(url)
    # Check if the request was successful
    if response.status_code == 200:
        # Parse the HTML content of the page
        soup = BeautifulSoup(response.text, "html.parser")

        # Find all occurrences of <div class="blog-post-body">
        divs = soup.find_all("div", class_="blog-post-body")

        # Extract the text between these divs and store it in a list
        text_list = [div.get_text(strip=True) for div in divs]

        # Print or manipulate the text as needed
        for text in text_list:
            date_pattern = r'(?:\w{3,9}\s\d{1,2}(?-\d{1,2}),\s\d{4})'

            # Find the first date-like substring in the input string
            match = re.search(date_pattern, text)
            # Extract the matched date
            if match:
                date = match.group()
                print(date)
            else:
                date = '1970'
            for state in us_dict:
                if text.find(state) != -1:
                    all.append([state,date,text])
    else:
        print("Failed to fetch the page. Status code:", response.status_code)

#%%
for i in range(15):
    count_masts("https://flagsexpress.com/flags-half-staff/?page="+str(i)
)
#%%        
count_masts("https://flagsexpress.com/flags-half-staff/?page="+str(1))

#%%
us_states = [
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New-Hampshire",
    "New-Jersey",
    "New-Mexico",
    "New-York",
    "North-Carolina",
    "North-Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode-Island",
    "South-Carolina",
    "South-Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West-Virginia",
    "Wisconsin",
    "Wyoming"
]
us_dict = {item: 0 for item in us_states}
# %%
import matplotlib.pyplot as plt

# Extract keys and values
keys = us_dict.keys()
values = us_dict.values()

# Create a histogram
plt.hist(masts)

# Add labels and title
plt.xlabel('Keys')
plt.ylabel('Values')
plt.title('Histogram of Key-Number Pairs')

# Show the histogram
plt.show()

#%%