# ZUM-Projekt

**Projekt z przedmiotu ZUM (Zaawansowane uczenie maszynowe)**

**Temat projektu:** Grupowanie (G)  
**Zestaw danych:** Dane FIFA 2019 Complete Player Dataset (Kaggle) -- grupowanie lub predykcja skuteczności zawodników.

**Autorzy:**
- Robert Wojtaś
- Jakub Sikora

**Treść zadania:**  
Konkretyzacja tego zadania wymaga:
- ustalenia atrybutów wykorzystywanych do grupowania (traktowanych jako obserwowalne) oraz ewentualnie zbioru atrybutów, na temat których będzie prowadzone wnioskowanie na podstawie przynależności do grup (traktowanych jako ukryte),
- określenia zakresu przygotowania danych (np. przetworzenia do odpowiedniej postaci tabelarycznej, modyfikacji typów/zbiorów wartości atrybutów, eliminacji/naprawy defektów danych, losowania prób),
- wskazania możliwości zdefiniowania nowych atrybutów (obserwowalnych),
- ustalenia kryteriów lub algorytmu selekcji atrybutów (obserwowalnych),
- wyboru algorytmów grupowania,
- wyboru miar niepodobieństwa przykładów (dla algorytmów, które je wykorzystują),
- wskazania parametrów algorytmów grupowania wymagających strojenia,
- ustalenia procedur i kryteriów oceny jakości modeli (z uwzględnieniem popularnych miar jakości grupowania oraz ewentualnie skuteczności predykcji atrybutów ukrytych),
- ustalenia sposobu opisu grup i charakteryzowania ich specyfiki na podstawie rozkładu wartości atrybutów.

**Zasady realizacji projektu:**  
- Wybór tematu projektu musi nastąpić do **końca piątego tygodnia semestru**. Niedokonanie wyboru tematu w tym terminie może być traktowane jako rezygnacja z wykonywania projektu.
- Do **końca siódmego tygodnia semestru** należy przedstawić wstępne założenia obejmujące:
szczegółową interpretację tematu projektu, opis algorytmów, które będą implementowane lub wykorzystane do badań,
plan badań, w tym:
  * cel poszczególnych eksperymentów (pytania, na które będzie poszukiwana odpowiedź, lub hipotezy do weryfikacji),
  * charakterystykę zbiorów danych, które będą wykorzystane (oraz ewentualnych czynności związanych z przygotowaniem danych),
  * parametry algorytmów, których wpływ na wyniki będzie badany,
  * miary jakości i procedury oceny modeli,
  * otwarte kwestie wymagające późniejszego rozwiązania (wraz z wyjaśnieniem powodów, dla których ich rozwiązanie jest odłożone na później).
- Do **końca przedostatniego tygodnia semestru** należy dostarczyć kod źródłowy opracowanego oprogramowania w formie elektronicznej, dokumentację kodu źródłowego w formie elektronicznej oraz dokumentację projektu zawierającą w poszerzonej i w razie potrzeby zmodyfikowanej postaci elementy wymienione wyżej dla założeń wstępnych oraz dodatkowo:
  * uzyskane wyniki,
  * dyskusję wyników i wnioski.

Na ocenę z projektu (100%) składają się:
- ocena założeń wstępnych (20%),
- ocena realizacji (80%).

Opóźnienie w oddaniu założeń wstępnych nie przekraczające jednego tygodnia powoduje przemnożenie uzyskanej oceny za założenia wstępne przez współczynnik 0.8. Po upływie jednego tygodnia od terminu założenia nie będą przyjmowane do oceny, a ocena późniejszych elementów projektu będzie automatycznie mnożona przez współczynnik 0.8.

Po upływie jednego tygodnia od terminu lub po zakończeniu zajęć dydaktycznych w semestrze kod i dokumentacja nie będą przyjmowane, z wyjątkiem uzasadnionych przypadków losowych. Opóźnienie w oddaniu któregokolwiek z produktów końcowych projektu (kodu źródłowego lub dokumentacji) nieprzekraczające jednego tygodnia powoduje przemnożenie uzyskanych ocen za realizację projektu przez współczynnik 0.8 (w przypadku braku założeń wstępnych łączny mnożnik wyniesie 0.8*0.8=0.64).

Przy ustalaniu numerów tygodni, o których mowa wyżej, uwzględnia się wyłącznie tygodnie, w których zgodnie z obowiązującym kalendarzem zajęć odbywa się wykład (tzn. pomijane są tygodnie, w których dzień wykładu jest dniem wolnym od zajęć dydaktycznych lub w dniu wykładu realizowane są zajęcia według rozkładu z innego dnia). Oznacza to w szczególności, że przez ostatni tydzień semestru rozumiany jest tydzień, w którym odbywa się ostatni wykład z ZUM, a przez przedostatni tydzień semestru -- tydzień, w którym odbywa się przedostatni wykład z ZUM.

**Dane:**  
link do [kaggle](https://www.kaggle.com/karangadiya/fifa19) 

**Wnioski po pierwszym spotkaniu:**  
- obserwowalne zwykłe atrybuty (odrzucamy overall na konkretnej pozycji), próbujemy pogrupować zawodników na podobnych pozycjach, 
- usunięcie niepotrzebnych i nadmiarowych (overall na konkretnej pozycji) atrybutów, uzupełnienie brakujących danych i skalowanie,
- możemy pokombinować z atrybutami opisującymi fizyczność zawodnika np BMI,
- na następnym spotkaniu wybierzemy algorytmy i opiszemy parametry z nimi związane oraz sformalizujemy cel/hipotezę.
