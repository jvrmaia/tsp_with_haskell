Diretórios:

* `fi0` : utiliza Binomial Heap para guardar os pontos fora do tour e uma Btree para o tour (KDT p/ nn)
* `fi1` : utiliza Binomial Heap para guardar os pontos fora do tour e uma KDTree para o tour e nn
* `fi2` : utiliza Btree para guardar os pontos fora do tour e uma Btree para o tour (KDT p/ nn)
* `fi3` : utiliza Btree para guardar os pontos fora do tour e uma KDTree para o tour e nn
* `fi4` : utiliza Pair Heap para guardar os pontos fora do tour e uma Btree para o tour (KDT p/ nn)
* `fi5` : utiliza Pair Heap para guardar os pontos fora do tour e uma KDTree para o tour e nn
* `dmst0` : tour from euller apartir de MST gerada com dijkstra utilizando a estrutura antiga
* `dmst1` : MST gerada com dijkstra utilizando a estrutura antiga
* `dmst2` : tour from euller apartir de MST gerada com dijkstra utilizando a estrutura sugerida pelo professor
* `dmst3` : MST gerada com dijkstra utilizando a estrutura sugerida pelo professor
* `dmst4` : tour from euller apartir de MST gerada com kruskall utilizando a estrutura sugerida pelo professor
* `dmst5` : MST gerada com kruskall utilizando a estrutura sugerida pelo professor

Como usar:

    $ cd <pasta>
    $ make
    $ ./main

