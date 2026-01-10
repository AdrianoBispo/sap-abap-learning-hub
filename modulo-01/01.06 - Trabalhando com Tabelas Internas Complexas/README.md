# **Módulo 01: Programação ABAP Básica**

## **Aula 06: Trabalhando com Tabelas Internas Complexas**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Distinguir tecnicamente entre os três tipos de tabelas internas: **Standard**, **Sorted** e **Hashed**, compreendendo a complexidade algorítmica de cada uma (Big O Notation).  
2. Utilizar **Expressões de Tabela** (lt_tab[ ... ]) para ler registros, realizar encadeamentos diretos e gerenciar exceções.  
3. Povoar e manipular tabelas rapidamente usando o operador construtor **VALUE**, incluindo a adição de linhas com BASE.  
4. Implementar e consumir **Chaves Secundárias** para otimizar buscas em tabelas Standard sem alterar sua estrutura primária.

### **1. Os Três Tipos de Tabelas Internas**

No ABAP, a escolha do tipo de tabela define como os dados são armazenados na memória e, crucialmente, como são acessados. Em grandes volumes de dados (ex: processamento de milhões de registros no S/4HANA), a escolha errada pode levar a problemas de performance graves (Timeouts).

#### **A. Standard Table (Tabela Padrão)**

É o tipo mais comum e flexível, mas também o mais perigoso se mal utilizado em buscas.

* **Estrutura:** Funciona como um Array ou Lista Encadeada lógica. A ordem das linhas é a ordem de inserção (APPEND).  
* **Acesso por Chave:** Utiliza **Busca Linear (Linear Search)**. O sistema percorre a tabela da primeira à última linha até encontrar o valor.  
  * *Complexidade:* **O(n)**. Se a tabela dobra de tamanho, o tempo de busca dobra.  
* **Uso Ideal:** Listas pequenas (< 100 linhas), buffers temporários onde a ordem de inserção importa, ou quando se usa sempre o índice (Index Access).  
* **Definição:** TYPE TABLE OF ... ou TYPE STANDARD TABLE OF ....

#### **B. Sorted Table (Tabela Ordenada)**

Mantém os dados automaticamente ordenados pela chave definida, inserção após inserção.

* **Estrutura:** Mantém uma árvore balanceada ou índice ordenado internamente.  
* **Acesso por Chave:** Utiliza **Busca Binária (Binary Search)**. O sistema divide a tabela ao meio repetidamente para encontrar o valor.  
  * *Complexidade:* **O(log n)**. Extremamente rápido mesmo em tabelas grandes.  
* **Custo:** A inserção (INSERT) é mais lenta que na Standard, pois o sistema precisa encontrar a posição correta para manter a ordem.  
* **Uso Ideal:** Tabelas que sofrem muitas leituras e poucas escritas, ou quando a ordem lógica dos dados é vital para o processamento (ex: FOR ALL ENTRIES).  
* **Definição:** TYPE SORTED TABLE OF ... WITH UNIQUE/NON-UNIQUE KEY ....

#### **C. Hashed Table (Tabela de Hash)**

O tipo mais performático para acesso por chave única.

* **Estrutura:** Utiliza um algoritmo de Hash para calcular o endereço de memória exato de um registro baseado na sua chave.  
* **Acesso por Chave:** Acesso Direto. Não importa se a tabela tem 10 ou 10 milhões de linhas, o tempo de resposta é teoricamente o mesmo.  
  * *Complexidade:* **O(1)** (Tempo Constante).  
* **Restrições:** A chave deve ser **UNIQUE**. Não é possível acessar pelo índice (linha 1, linha 2), pois não existe ordem sequencial lógica.  
* **Uso Ideal:** Caches, Tabelas de Mestre (Clientes, Materiais) onde o acesso é sempre pelo ID único.  
* **Definição:** TYPE HASHED TABLE OF ... WITH UNIQUE KEY ....

### **2. Sintaxe Moderna: Expressões de Tabela**

As expressões de tabela (itab[ ... ]) modernizam a leitura de linhas, aproximando o ABAP de linguagens como Java ou Python. Elas não retornam sy-subrc, mas sim o resultado direto ou uma exceção.

#### **Leitura e Tratamento de Exceção**

Diferente do READ TABLE que apenas define sy-subrc = 4 quando falha, a expressão de tabela lança uma exceção CX_SY_ITAB_LINE_NOT_FOUND.

* **Antigo (Read Table):**  
  READ TABLE lt_flights INTO ls_flight WITH KEY carrier_id = 'LH'.  
  IF sy-subrc = 0.   
    " Processar  
  ENDIF.

* **Moderno (Table Expression):**  
  TRY.  
      DATA(ls_flight) = lt_flights[ carrier_id = 'LH' ].  
      " Processar ls_flight...  
  CATCH cx_sy_itab_line_not_found.  
      " Tratar erro se não achar (Opcional, se a lógica exigir existência)  
  ENDTRY.

#### **Encadeamento (Chaining)**

Uma grande vantagem é poder acessar um campo ou sub-estrutura diretamente, sem precisar copiar a linha inteira para uma work area.

" Acessa o campo 'price' da linha onde id = '123'  
DATA(lv_price) = lt_products[ id = '123' ]-price.

" Acessa uma tabela aninhada dentro da estrutura  
DATA(lv_seat) = lt_flights[ 1 ]-bookings[ customer = 'SAP' ]-seat_num.

#### **Funções Predicativas: line_exists e line_index**

Para verificar existência ou obter o número da linha sem causar exceções.

* IF line_exists( lt_tab[ key = val ] ).: Substitui o READ ... TRANSPORTING NO FIELDS.  
* DATA(lv_idx) = line_index( lt_tab[ key = val ] ).: Retorna o índice (sy-tabix) ou 0 se não achar.

### **3. Operador VALUE para Tabelas**

O operador VALUE permite construção em massa e também a adição de linhas a tabelas existentes usando a cláusula BASE.

#### **Inicialização (Criação Nova)**

DATA(lt_currencies) = VALUE ty_currencies_tab(  
  ( code = 'EUR' name = 'Euro' )  
  ( code = 'USD' name = 'Dólar' )  
).

#### **Append e Merge (Usando BASE)**

Sem o BASE, o comando VALUE limpa a tabela antes de inserir. Com BASE, ele anexa.

" Adiciona 'BRL' à lista existente de moedas  
lt_currencies = VALUE #(   
    BASE lt_currencies   
    ( code = 'BRL' name = 'Real' )   
).

### **4. Exemplo Prático: Comparando Tipos e Chaves Secundárias**

Neste exemplo avançado, vamos demonstrar uma tabela **Hashed** para performance e o uso de uma **Chave Secundária** em uma tabela Standard para acelerar uma busca alternativa.

CLASS zcl_itab_demo DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if_oo_adt_classrun .  
      
    TYPES: BEGIN OF ty_product,  
             id       TYPE string,  
             category TYPE string,  
             name     TYPE string,  
             price    TYPE p LENGTH 10 DECIMALS 2,  
           END OF ty_product.

    " Tabela Hashed: Otimizada para busca por ID (Chave Primária)  
    " Adicionamos uma Chave Secundária Ordenada para buscar rápido por Categoria  
    TYPES ty_prod_hashed_table TYPE HASHED TABLE OF ty_product   
                               WITH UNIQUE KEY id  
                               WITH NON-UNIQUE SORTED KEY sk_cat COMPONENTS category.

  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl_itab_demo IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " 1. Populando a tabela com VALUE  
    DATA(lt_products) = VALUE ty_prod_hashed_table(  
      ( id = 'P001' category = 'HW' name = 'Notebook'  price = '3500.00' )  
      ( id = 'P002' category = 'AC' name = 'Mouse'     price = '50.00' )  
      ( id = 'P003' category = 'HW' name = 'Monitor'   price = '1200.00' )  
      ( id = 'P004' category = 'SW' name = 'Windows'   price = '800.00' )  
    ).

    " 2. Acesso Primário (HASH - O(1))  
    " Busca extremamente rápida pelo ID  
    IF line_exists( lt_products[ id = 'P002' ] ).  
       out->write( 'Produto P002 encontrado via Hash Key.' ).  
    ENDIF.

    " 3. Acesso Secundário (SORTED - O(log n))  
    " Se buscássemos por categoria numa tabela Hashed sem chave secundária, seria Linear O(n).  
    " Usando a chave 'sk_cat', o acesso vira Binário O(log n).  
      
    " Nota: LOOP com USING KEY é vital para performance em filtros secundários  
    LOOP AT lt_products INTO DATA(ls_prod) USING KEY sk_cat WHERE category = 'HW'.  
        out->write( |Produto de Hardware: { ls_prod-name }| ).  
    ENDLOOP.

    " 4. Modificação Direta com Expressão e VALUE  
    " Atualizando preço do Monitor  
    TRY.  
        lt_products[ id = 'P003' ]-price = 1100.  
        out->write( |Novo preço do Monitor: { lt_products[ id = 'P003' ]-price }| ).  
    CATCH cx_sy_itab_line_not_found.  
        out->write( 'Erro ao atualizar.' ).  
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Standard Table:** Tipo de tabela interna onde o acesso por chave não é otimizado (Busca Linear - O(n)). Ideal para acesso via índice ou APPEND simples.  
* **Hashed Table:** Tipo de tabela interna otimizada para acesso via chave única (Algoritmo de Hash - O(1)). Ideal para grandes volumes de dados únicos.  
* **Sorted Table:** Tipo de tabela interna mantida sempre ordenada (Busca Binária - O(log n)). Ideal para buscas por intervalo ou chave parcial.  
* **Secondary Key (Chave Secundária):** Um índice adicional criado sobre uma tabela interna que permite buscas rápidas por campos que não compõem a chave primária. Pode ser SORTED ou HASHED.  
* **Table Expression (itab[...]):** Sintaxe funcional para leitura de registros. Lança exceção cx_sy_itab_line_not_found em caso de falha, ao contrário do sy-subrc.  
* **Big O Notation:** Notação matemática usada para descrever a eficiência de um algoritmo. O(1) é o melhor (constante), O(n) é linear (proporcional ao tamanho).

#### **Pontos de Atenção (Performance e Sintaxe)**

| Conceito | ABAP Legado | ABAP Moderno | Impacto |
| :---- | :---- | :---- | :---- |
| **Ler Linha** | READ TABLE tab WITH KEY k=v INTO wa. | wa = tab[ k = v ]. | Sintaxe mais limpa e direta. |
| **Verificar** | READ TABLE ... TRANSPORTING NO FIELDS. | IF line_exists( tab[...] ). | Código mais legível. |
| **Obter Índice** | READ TABLE ... depois verificar sy-tabix | idx = line_index( tab[...] ). | Acesso direto ao metadado. |
| **Inserir/Adicionar** | APPEND wa TO tab. | tab = VALUE #( BASE tab ( ... ) ). | Construção em massa eficiente. |
| **Busca Lenta** | LOOP WHERE em Tabela Standard | LOOP USING KEY Secundária | Transforma O(n) em O(log n) ou O(1). |

### **📝 Quiz de Fixação**

Q1: Qual tipo de tabela interna garante tempo de acesso constante (O(1)), independentemente do número de registros, mas exige uma chave única?  
R: Hashed Table. Ela utiliza um algoritmo de hash para calcular a posição exata do registro na memória.  
Q2: O que acontece se eu tentar acessar lt_tabela[ id = '999' ] e o registro não existir?  
R: O sistema lançará uma exceção da classe cx_sy_itab_line_not_found. Se não for tratada com TRY...CATCH, causará um Short Dump (erro em tempo de execução).  
Q3: Tenho uma tabela Standard gigante de "Vendas" e preciso filtrar frequentemente por "Data", que não é a chave primária. Como otimizar sem mudar o tipo da tabela?  
R: Definindo uma Chave Secundária (WITH NON-UNIQUE SORTED KEY sk_data COMPONENTS data) na definição da tabela e utilizando USING KEY sk_data nas leituras e loops.  
Q4: Qual é o custo ("trade-off") de usar uma Tabela Sorted em comparação com uma Standard?  
R: A operação de inserção (INSERT) é mais lenta na Tabela Sorted, pois o sistema precisa encontrar a posição correta para manter a ordenação. Na Standard, o APPEND apenas adiciona ao final, o que é instantâneo.