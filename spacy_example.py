# Install: pip install spacy && python -m spacy.en.download
import spacy

nlp = spacy.load('en')
doc = nlp(u'The food was terrible.')

for word in doc:
    print(word.text, word.lemma, word.lemma_, word.tag, word.tag_, word.pos, word.pos_)
    