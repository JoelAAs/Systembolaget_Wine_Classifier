# Systembolaget_webcrawl

This program is able to create a simple database of the red wines at Systembolaget, 
and based on scores given to wines, it will predict scores to other wines based on 
taste keywords, grapes, country and other factors.

Two different scorings systems are used.
Mean of each meaningfull taste keyword from Systembolagets reviews from previously 
scored wines, on a scle from 0-100 where a score of 50 is mediocre. This mean is 
combined with the mean score of that region, and grape. 

The frequency scoring system is based on the negative logarithm of the frequency of
matches between combinations of taste keywords. The logic of this is that if a combination
of keywords from one wine rarely matches another then the score of that match
 should reflect the "uniqness" of that wine.

The main analysis part of the program is written in R (under Rcode) and the webcrawler
is written in python (under python). The latest scrapped data is under Data.

Alla+Artiklar.csv is a CSV version of the "Sortimentsfilen" XML file available as part
of Systembolaget's open API. It is part of Systembolagets intellectual property, as
described on http://www.systembolaget.se/Tjanster/Oppna-APIer (in swedish).
