#!/usr/bin/python
# -*- coding: utf-8 -*-
""" Downloads XML from Systembolaget's api and crawls Systembolaget for key-review words """
from datetime import datetime
from xml.etree import ElementTree
import os
import csv
import requests
import re
import unicodedata
from bs4 import BeautifulSoup
__author__ = "Joel Ås"


wine_db_location = "./data/wine_database.csv"
info_file_location = "./data/info.txt"
latest_wine_db_location = "./data/latest_db.csv"
latest_wine_db_location_xml = "./data/latest_db.xml"
previous_id = [None]


def list_replace(input_str, match, replace):
    """surely there is a function for this but fuck it """
    for i in range(0, len(match)):
        input_str = input_str.replace(match[i], replace[i])
    return input_str


def set_previous_id():
    """ Sets the global array of article id in database """
    global previous_id
    previous_id = [None] * sum(1 for line in open(wine_db_location))
    n = 0
    with open(wine_db_location) as wine_db:
        reader = csv.DictReader(wine_db)
        for row in reader:
            previous_id[n] = row['Varnummer']
            n += 1


def sanitize_taste(taste_string):
    """ Removes fill-words and clean up data """
    taste_string = unicodedata.normalize("NFKD", taste_string).encode("ascii", "ignore")
    regexp = re.compile("(.*?)Serveras")
    taste_string = regexp.match(taste_string).group().replace(". Serveras", "")
    matches = ["med",
               "och",
               "inslag av",
               "smak",
               "doft",
               ","]
    replacements = [", ",
                    ", ",
                    ", ",
                    " ",
                    " ",
                    "."]
    taste_string = list_replace(taste_string,
                                matches,
                                replacements)
    regexp = re.compile("[ ][ ]+")
    taste_string = regexp.sub("", taste_string)
    regexp = re.compile("\.\.+")
    taste_string = regexp.sub(".", taste_string)
    regexp = re.compile("[ ]+\.[ ]+|[ ]+\.|\.[ ]+|\.+")
    taste_string = regexp.sub(".", taste_string)
    return taste_string


def get_review_and_clocks(article_id):
    """ Downloads systembolaget page for wine via article ID"""
    start_url = "http://www.systembolaget.se/Sok-dryck/Dryck/?varuNr="
    target_url = start_url + article_id
    page_code = requests.get(target_url)
    print(target_url)
    page_as_text = page_code.text
    wine_soup = BeautifulSoup(page_as_text, "lxml")
    taste_keys = "inte testat"
    taste_clocks = wine_soup.findAll("span", {"class": "cmp-screen-reader-text"})
    skip = False
    try:
        fyllighet = re.search("[0-9]+", taste_clocks[2].text).group()
        stravhet = re.search("[0-9]+", taste_clocks[3].text).group()
        fruktsyra = re.search("[0-9]+", taste_clocks[4].text).group()
    except (AttributeError, IndexError):
        fyllighet = ""
        stravhet = ""
        fruktsyra = ""
    desc = wine_soup.findAll("p", {"class": "description"})
    try:
        taste_keys = sanitize_taste(desc[0].text)
    except (IndexError, AttributeError):
        print("No review, skipping")
        skip = True
    if skip:
        return None
    else:
        return fyllighet + "," + stravhet + "," + fruktsyra + "," + taste_keys


def create_wine_csv(csv_file, artikel, keys):
    """ Selects all nodes with varugrupp == 'Rött vin' and writes it to given file (CSV) """
    varugrupp = artikel.find('Varugrupp').text
    arid = artikel.find('Varnummer').text
    sort = artikel.find("Sortiment").text
    if varugrupp != u"Rött vin":
        return 0
    elif arid not in previous_id and sort == u"FS":  # check if in DB already and make sure its ordenary sortiment
        print("importing " + arid + " av typ " + varugrupp + "not present in DB, adding..")
        new_line = ""
        for key in keys:
            try:
                new_data = artikel.find(key).text
                new_data = new_data.replace(",", " ")
                new_line += new_data + ","
            except TypeError:
                new_line += ","
            except AttributeError:
                new_line += ","
        review = get_review_and_clocks(arid)
        if review is None:  # Not interesting if there is no review
            return 0
        else:
            new_line += review
            new_line += "\n"
            csv_file.write(new_line.encode('utf8'))
    else:
        return 0


def import_xml_into_db():
    """ Downloads and selects all data to crawl from xml """
    xml_url = "https://www.systembolaget.se/api/assortment/products/xml"
    bash_cmd = "wget --output-document=" + latest_wine_db_location_xml + " " + xml_url
    os.system(bash_cmd)
    dom = ElementTree.parse(latest_wine_db_location_xml)
    artiklar = dom.findall("artikel")
    keyz = ["nr",
            "Artikelid",
            "Varnummer",
            "Namn",
            "Namn2",
            "Prisinklmoms",
            "Pant",
            "Volymiml",
            "PrisPerLiter",
            "Saljstart",
            "Slutlev",
            "Varugrupp",
            "Forpackning",
            "Forslutning",
            "Ursprung",
            "Ursprunglandnamn",
            "Producent",
            "Leverantor",
            "Argang",
            "Provadargang",
            "Alkoholhalt",
            "Sortiment",
            "Ekologisk",
            "Koscher",
            "RavarorBeskrivning"]
    set_previous_id()
    time_updated = dom.findall("skapad-tid")
    info_file = open('./data/info.txt', 'w+')
    info_file.write(time_updated[0].text + "\n")
    info_file.close()
    if not os.path.isfile(wine_db_location):  # If database doesn't exist create one
        wine_file = open(wine_db_location, "w+")
        wine_file.write(",".join(keyz) + "fyllighet,stravhet,fruktsyra,smak")
    else:
        wine_file = open(wine_db_location, "a")
    for artikeln in artiklar:
        create_wine_csv(wine_file, artikeln, keyz)
    wine_file.close()


def latest_update():
    """ Checking when the database was updated and ask user if an update is desired """
    if os.path.isfile(info_file_location):
        current_date = datetime.now()
        xml_update_time = datetime.now()
        err = True
        with open(info_file_location) as f:
            for line in f:
                try:
                    xml_update_time = datetime.strptime(line.replace("\n", ""), "%Y-%m-%d %H:%M")
                except ValueError:
                    err = False
        if err:
            time_since_update = current_date - xml_update_time
            print ("The last update to the database was " +
                str(time_since_update.days) + " days ago. \n")
            user_input = str(input("Enter \"Y\" to check for new update: "))
            if user_input == "Y":
                import_xml_into_db()
        else:
            print("Something is wrong with info.txt, updating...")
            import_xml_into_db()
    else:
        import_xml_into_db()


if __name__ == "__main__":
    latest_update()
