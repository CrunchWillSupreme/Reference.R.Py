# Please install html2text, requests, textblob modules using pip install

from textblob import TextBlob
import requests
import html2text

page = requests.get("https://careers.homedepot.com/job/7532414/financial-analyst-decision-analytics-atlanta-ga/")


blob = TextBlob(html2text.html2text(page.text))
print(blob)
print(blob.tags)
tags = blob.tags

#html_content = tags.text
#print(html_content)
#tags = html_content.tags
#    soup = BeautifulSoup(tags, 'lxml')  

#blobtext=page.text
#print(blobtext)
#soup=BeautifulSoup(blobtext, 'lxml')
#print(soup)
#print(soup.get_text())
print(blob.noun_phrases)

for sentence in blob.sentences:
    print(sentence)
    print(sentence.sentiment.polarity)
