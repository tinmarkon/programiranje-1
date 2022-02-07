# =============================================================================
# Ker ne zna ničesar koristnega, se je Miha odločil, da bo postal vplivnež. 
# Priskrbel si je zemljevid plaže, ki za vsako mesto na plaži
# pove, koliko sledilcev dobi (ali izgubi), če objavi fotografijo s tega mesta. 
# Plažo predstavimo s pravokotno mrežo dimenzije `M * N`, kjer za vsako celico 
# povemo, koliko sledilcev bo Miha dobil, če se na poti prek te celice slika.
# Miha svojo pot začne v točki `(0, 0)`, konča v točki `(M-1, N-1)`, na svoji 
# poti do cilja pa bi rad nabral čim več sledilcev, pri čemer med potjo nikoli 
# ne sme zaiti izven plaže.
# Miha se lahko običajno premika na tri načine: korak desno, korak navzdol, 
# korak desno-navzdol in pri tem objavi slike iz vseh lokacij
# na svoji poti (tudi če so njihove vrednosti negativne). Poleg osnovnih korakov 
# lahko "največ enkrat" na svoji poti naredi tudi 
# korak nazaj (torej se vrne na polje, kjer je bil trenutek prej). 
# Ker sledilci nimajo dobrega spomina, se lahko Miha večkrat slika na isti lokaciji 
# in vsakič dobi (ali izgubi) podano število sledilcev.
# 
# Definirajte funkcijo, ki sprejme zemljevid plaže in vrne maksimalno število sledilcev, 
# ki jih Miha lahko nabere na podani plaži.
# Miho zanima zgolj končna sprememba sledilcev, zato je ta lahko skupno tudi negativna.
# 
# Na spodnji mreži je najvplivnejši sprehod (1, 2, 5, 30, 5, 30, -1, 5) vreden 77 sledilcev. 
# =============================================================================

piran = [
    [1, 2,  -3, -10, 9],
    [0, 0,   5,   5, 2],
    [1, 2,  30,  -1, 0],
    [4, 3, -20,  -1, 5],
]


def pot_najvec_obiskovalcev (matrika): 
    row, column = len(matrika), len(matrika[0])
    sledilci = [[None for _ in range(column)] for _ in range(row)]
    for i in range(row - 1, -1, -1):
        for j in range(column - 1, -1, -1):
            if i < (row - 1) and j < (column - 1):
                sledilci[i][j] = matrika[i][j] + max(sledilci[i + 1][j], sledilci[i][j + 1], sledilci[i + 1][j + 1])
            elif i < (row - 1): # smo v zadnjem stolpcu
                sledilci[i][j] = matrika[i][j] + sledilci[i + 1][j]
            elif j < (column - 1): # smo v zadnji vrstici
                sledilci[i][j] = matrika[i][j] + sledilci[i][j + 1]
            else:
                sledilci[i][j] = matrika[i][j]
    return sledilci[0][0]


def najdrazja_pot_iz_polja(matrika, i, j):
    row, column = len(matrika), len(matrika[0])
    if i < (row - 1) and j < (column - 1):
        cena_in_pot_desno = najdrazja_pot_iz_polja (matrika, i, j + 1)
        cena_in_pot_dol = najdrazja_pot_iz_polja (matrika, i + 1, j)
        cena_in_pot_diag = najdrazja_pot_iz_polja (matrika, i + 1, j + 1)
        max_cena, max_pot = max (cena_in_pot_desno, cena_in_pot_dol, cena_in_pot_diag)
        return matrika[i][j] + max_cena, [matrika[i][j]] + max_pot
    elif i < (row - 1): # j = column - 1 -> smo v zadnjem stolpcu #
        cena_dol, pot_dol = najdrazja_pot_iz_polja (matrika, i + 1, j)
        return matrika[i][j] + cena_dol, [matrika[i][j]] + pot_dol
    elif j < (column - 1): # i = row - 1 -> smo v zadnji vrstici #
        cena_desno, pot_desno = najdrazja_pot_iz_polja (matrika, i, j + 1)
        return matrika[i][j] + cena_desno, [matrika[i][j]] + pot_desno
    else: # Pogoj implicira da sta j = column - 1 in i = row - 1 #
        return matrika[i][j], [matrika[i][j]]


# Očitno je da če se lahko vrnemo, potem je smiselno gledati takšna podzaporedja dveh elementov, katerih vsota je največja.
# Isti par dodamo za njima.

# Ali je možna implementacija direktno v algoritem s pomočjo joker karte ?? 


def maksimalni_par(seznam):
    dvojice = [[(seznam[j], seznam[j + 1], seznam[j] + seznam[j + 1])] for j in range(0, len(seznam)- 1)]
    return dvojice

def dodaj_max_takoj(seznam):
    max_levi, max_desni, _ = maksimalni_par (seznam)
    

delna_resitev = najdrazja_pot_iz_polja (piran, 0 ,0)

maksimalni_par (delna_resitev[1])