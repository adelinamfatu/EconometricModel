# Proiect Econometrie - Modele econometrice

## APLICATIA I: Analiza factorilor care influențează prețul caselor din București

Aplicația studiază influențele asupra prețului imobilelor din București la nivelul anului 2019. Este folosit software-ul RStudio și limbajul și mediul R pentru calcul statistic și grafică.

### Modelul de regresie liniară simplă:
𝑌=𝛼+𝛽𝑋
unde:
- Y – variabila dependentă, prețul imobilului
- X – variabila independentă, suprafața imobilului

### Modelul de regresie liniară multiplă:
𝑌=𝛼+𝛽1∗𝑋1+𝛽2∗𝑋2+𝛽3∗𝑋3
unde:
- X1 – suprafața imobilului
- X2 – totalul etajelor clădirii în care se află imobilul
- X3 – etajul la care se află imobilul

Pentru testarea validității modelului am calculat indicatorii de bonitate, aceștia fiind coeficientul de determinație, coeficientul de nedeterminație, coeficientul de determinație ajustat, raportul de corelație. Totodată, am testat ipotezele modelului de regresie liniară și semnificația parametrilor din model.

În cadrul aplicației sunt utilizate următoarele teste:
- Homoschedasticitate:
  - Testul Breusch-Pagan
  - Testul White
- Autocorelare:
  - Testul Durbin-Watson
  - Testul Breusch-Godfrey
- Normalitate:
  - Testul Jarque-Bera
  - Testul Shapiro-Wilk

Modelul economic care descrie factorii determinanți ai prețului unei locuințe din București este: 𝑃𝑟𝑒𝑡=𝑓(𝑁𝑟𝐶𝑎𝑚𝑒𝑟𝑒,𝑆𝑢𝑝𝑟𝑎𝑓𝑎𝑡𝑎,𝐸𝑡𝑎𝑗,𝑇𝑜𝑡𝑎𝑙𝐸𝑡𝑎𝑗𝑒,𝑆𝑒𝑐𝑡𝑜𝑟,𝑆𝑐𝑜𝑟)

## APLICATIA II: Speranța de viață în perioada 2005-2015 din Europa

Aplicația studiază indicele fericirii la nivel global și alți factori ce posibil îl influențează. Este folosit software-ul RStudio și limbajul și mediul R pentru calcul statistic și grafică, la fel ca aplicația 1.

În aplicație sunt utilizate următoarele teste:
- Determinarea dacă se folosesc estimatori cu efecte fixe sau OLS:
  - Testul F
- Determinarea dacă se folosesc estimatori cu efecte fixe sau efecte random:
  - Testul Hausman

Modelul economic folosit este următorul: 𝑆𝑝𝑒𝑟𝑎𝑛𝑡𝑎 𝑙𝑎 𝑣𝑖𝑎𝑡𝑎=𝑓(𝑃𝑜𝑝𝑢𝑙𝑎𝑡𝑖𝑒,𝐸𝑚𝑖𝑠𝑖𝑖_𝐶𝑂2,𝐶ℎ𝑒𝑙𝑡𝑢𝑖𝑒𝑙𝑖_𝑆𝑎𝑛𝑎𝑡𝑎𝑡𝑒,𝐶𝑜𝑛𝑠𝑢𝑚_𝐸𝑙𝑒𝑐𝑡𝑟𝑖𝑐𝑖𝑡𝑎𝑡𝑒)

