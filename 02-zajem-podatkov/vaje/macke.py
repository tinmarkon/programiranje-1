import csv
import os
import requests
from requests import status_codes
from requests.models import HTTPError
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'https://www.bolha.com/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'C:/Users/TinM/Documents/Studij/Programiranje 1/programiranje-1/02-zajem-podatkov/vaje/'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'cat-frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat.csv'


def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        page_content = requests.get(cats_frontpage_url)
    except requests.exceptions.ConnectionError as e:
        print(f"Napaka pri povezovanju do: {cats_frontpage_url}: \n{e}")
        return None
    if page_content.status_code == requests.codes.ok:
        return page_content.text
    else:
        raise requests.HTTPError(f"Ni ok: {frontpage_filename.status_code}")

def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    page_content = download_url_to_string(page)
    save_string_to_file(page_content, directory, filename)
    


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file:
        return file.read()



# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne seznam oglasov."""
    sample = re.compile(r'<li class="EntityList-item EntityList-item--[^Latest].*?' #Latest so zadnji oglasi nasploh na bolhi, uporabim 
                r'</article>'
                r'.*?</li>',
                re.DOTALL)
    ads = re.findall(sample, page_content)
    return ads


# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke."""
    sample = re.compile(r'data-href="\/(?P<kategorija>\S*)'
                        r'\/(?P<naslov>\S*)"'
                        r'.*?<span class="entity-description-itemCaption">Lokacija: </span>(?P<Lokacija>\w*)'
                        r'.*?datetime="(?P<datum>\S*)"'
                        r'.*?<strong class="price price--hrk">\s*(?P<cena>Cena po dogovoru|\S*)',
                        re.DOTALL)
    data = re.search(sample, block)
    ad_dict = data.groupdict()
    return ad_dict
        


    

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    frontpage = read_file_to_string(directory, filename)
    blocks = page_to_ads(frontpage)
    ads = [get_dict_from_ad_block(block) for block in blocks]
    return ads



###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    slovarjev parametra ads enaki in je seznam ads neprazen."""
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    write_csv(ads[0].keys(), ads, directory, filename)


# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran
    save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename)
    
    # Iz lokalne (html) datoteke preberemo podatke
    string = read_file_to_string(cat_directory, frontpage_filename)
    ads = page_to_ads(read_file_to_string(cat_directory, frontpage_filename))
    

    # Podatke preberemo v lepšo obliko (seznam slovarjev)
    ads_dict = [get_dict_from_ad_block(ad) for ad in ads]


    # Podatke shranimo v csv datoteko
    write_cat_ads_to_csv(ads_dict, cat_directory, frontpage_filename)

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
    # in enako za pretvorbo

    #raise NotImplementedError()


if __name__ == '__main__':
    main()
