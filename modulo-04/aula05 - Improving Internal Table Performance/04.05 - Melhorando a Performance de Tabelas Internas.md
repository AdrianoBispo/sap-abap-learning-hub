# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 05: Melhorando a Performance de Tabelas Internas**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Compreender profundamente a **Complexidade Algorítmica** (Notação Big O) aplicada às estruturas de dados ABAP, diferenciando o custo computacional de buscas Lineares O(n), Binárias O(log n) e Hash O(1).  
2. Definir, implementar e consumir **Chaves Secundárias** (SECONDARY KEYS) para acelerar drasticamente buscas em tabelas Standard sem alterar a lógica principal de ordenação.  
3. Gerenciar a memória do servidor de aplicação eficientemente, evitando cópias desnecessárias de linhas largas ("Wide Tables") através do uso de **Field-Symbols** (ASSIGNING) e **Referências de Dados** (REFERENCE INTO).  
4. Identificar, diagnosticar e refatorar o antipadrão de **Nested Loops** (Loops Aninhados) em tabelas Standard, transformando operações de complexidade quadrática em lineares ou logarítmicas.

### **1\. O Custo Oculto da Busca Linear**

O erro de performance mais comum e devastador no desenvolvimento ABAP é realizar operações de leitura (READ TABLE ou LOOP ... WHERE) em uma **Tabela Standard** sem utilizar uma chave otimizada. Muitas vezes, esse erro passa despercebido em ambientes de desenvolvimento com poucos dados, mas causa **Timeouts** (erros de tempo limite) catastróficos em produção.

#### **Entendendo o "Table Scan" em Memória**

Uma Tabela Standard não possui índice interno para buscas por conteúdo; ela é organizada apenas pela ordem de inserção (como uma lista de compras).

* **Busca Linear (Linear Search):** Quando você pede ao sistema "Busque a linha onde Matéria \= 'X'", o kernel do ABAP começa na linha 1, verifica se é 'X'. Se não for, vai para a linha 2, e assim por diante.  
  * **Complexidade O(n):** O tempo de busca cresce linearmente com o volume de dados.  
  * **Cenário Pequeno:** Em 100 linhas, é instantâneo (microssegundos).  
  * **Cenário Real:** Em 1.000.000 de linhas, o sistema pode levar segundos inteiros apenas para encontrar *um* registro. Se essa busca estiver dentro de um loop, o programa irá travar.

**Regra de Ouro:** Se você vai ler uma tabela interna muitas vezes por uma chave específica (ex: dentro de um loop ou múltiplas vezes num processo), essa tabela **jamais** deveria ser acessada de forma linear. Ela deve ser tipada como Sorted/Hashed ou possuir uma Chave Secundária.

### **2\. A Salvação: Chaves Secundárias**

Muitas vezes, recebemos uma tabela Standard de um método de classe global ou BAPI e não podemos mudar seu tipo base (pois quebraria outros consumidores). No entanto, o ABAP moderno permite adicionar **Chaves Secundárias** na definição local da variável.

Isso cria um "índice auxiliar" paralelo na memória RAM. O sistema mantém a tabela original intacta, mas constrói uma árvore de busca ou mapa de hash adicional que aponta para as linhas da tabela principal.

#### **Tipos de Chaves Secundárias**

* **SORTED KEY:** Cria um índice ordenado (árvore binária). Permite buscas parciais (ex: buscar apenas pelo primeiro campo da chave) e buscas por intervalo (Range).  
* **HASHED KEY:** Cria um mapa de hash. Permite apenas buscas exatas pela chave completa, mas é imbatível em velocidade (O(1)).

#### **Definição e Sintaxe**

TYPES: BEGIN OF ty\_data,  
         id       TYPE i,  
         category TYPE string,  
         name     TYPE string,  
       END OF ty\_data.

" Tabela Standard (acesso via índice rápido) mas com um índice rápido por ID  
" UNIQUE: Garante que não haverá IDs duplicados (o sistema valida na inserção)  
" NON-UNIQUE: Permite duplicatas (comum para categorias, status, etc.)  
TYPES ty\_t\_data TYPE STANDARD TABLE OF ty\_data  
                WITH DEFAULT KEY  
                WITH UNIQUE HASHED KEY key\_id COMPONENTS id  
                WITH NON-UNIQUE SORTED KEY key\_cat COMPONENTS category.

#### **Uso Obrigatório: USING KEY**

Para que o kernel ABAP utilize o índice secundário, você deve ser explícito. Se omitir, ele fará a busca linear padrão.

" Acesso Ultra-Rápido via Chave Secundária Hashed (Busca por ID)  
DATA(ls\_row) \= lt\_data\[ KEY key\_id COMPONENTS id \= 500 \].

" Loop Otimizado via Chave Secundária Sorted (Busca por Categoria)  
" Sem o 'USING KEY', este loop varreria a tabela inteira linha a linha.  
" Com 'USING KEY', ele salta direto para o primeiro registro da categoria 'A'  
LOOP AT lt\_data ASSIGNING FIELD-SYMBOL(\<fs\_row\>)   
                USING KEY key\_cat   
                WHERE category \= 'Hardware'.  
ENDLOOP.

### **3\. Cópias vs. Referências: Gerenciando a Memória**

Quando iteramos sobre uma tabela interna, o ABAP precisa disponibilizar o conteúdo da linha atual para o processamento. Existem duas formas de fazer isso, com impactos de memória drasticamente diferentes.

#### **A. Cópia (INTO)**

Cria uma duplicata física da linha na memória ("Work Area").

* **Mecanismo:** O sistema aloca um novo bloco de memória e copia byte a byte o conteúdo da linha da tabela para a variável DATA.  
* **Prós:** Segurança. Se você alterar a Work Area, a tabela original permanece intacta.  
* **Contras:** Performance degradada em "Wide Tables" (tabelas com centenas de colunas ou strings longas). Alocar e desalocar memória milhões de vezes custa CPU (Garbage Collector).

" Cria uma cópia isolada. Lento para linhas largas.  
LOOP AT lt\_big\_table INTO DATA(ls\_copy).   
  " ls\_copy é independente da tabela.  
ENDLOOP.

#### **B. Referência (ASSIGNING FIELD-SYMBOL)**

Cria um ponteiro (Pointer) direto para a linha na memória da tabela.

* **Mecanismo:** Não há alocação de nova memória para dados. O Field-Symbol aponta para o endereço onde a linha já está.  
* **Prós:** Velocidade máxima. Permite alterar o conteúdo da tabela diretamente sem precisar de MODIFY.  
* **Contras:** Cuidado ao modificar campos que são chaves de tabelas Sorted/Hashed (pode causar Dump se quebrar a ordenação).

" Acesso direto à memória. Extremamente rápido.  
LOOP AT lt\_big\_table ASSIGNING FIELD-SYMBOL(\<fs\_row\>).   
  \<fs\_row\>-status \= 'X'. " Modifica a tabela instantaneamente.  
ENDLOOP.

#### **C. Referência de Objeto (REFERENCE INTO)**

Uma alternativa moderna ao Field-Symbol, que usa referências de dados OO (REF TO). É ligeiramente mais segura que ponteiros puros e útil em contextos OO.

LOOP AT lt\_big\_table REFERENCE INTO DATA(lr\_row).  
  lr\_row-\>status \= 'X'. " Acesso via desreferenciação (-\>)  
ENDLOOP.

### **4\. O Perigo do Loop Aninhado (Nested Loop)**

O cenário clássico de performance ruim é cruzar dados de duas tabelas (ex: Pedidos e Clientes).

* Abordagem Ingênua (Quadrática \- O(N\*M)):  
  Loop na Tabela A (10.000 linhas). Para cada linha, ler Tabela B (5.000 linhas) de forma linear.  
  * *Cálculo:* 10.000 x 5.000 \= **50.000.000 (50 Milhões)** de comparações. Isso vai travar o processo.  
* Abordagem Otimizada (Logarítmica/Linear):  
  Loop na Tabela A. Para cada linha, ler Tabela B usando uma Chave Hashed ou Sorted.  
  * *Custo Hashed:* 10.000 x 1 \= **10.000** operações.  
  * *Custo Sorted:* 10.000 x log(5000) ≈ **120.000** operações.  
  * *Ganho:* A abordagem otimizada é cerca de **5.000 vezes mais rápida**.

### **5\. Exemplo Prático: Benchmark de Performance**

Neste exemplo expandido, vamos simular uma carga de dados massiva e comparar cronometricamente a leitura linear versus a leitura com chave secundária, provando o ganho real.

CLASS zcl\_perf\_demo DEFINITION  
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun.  
      
    TYPES: BEGIN OF ty\_material,  
             matnr TYPE string,  
             desc  TYPE string,  
             price TYPE p LENGTH 10 DECIMALS 2,  
           END OF ty\_material.

    " Tabela Híbrida: Comporta-se como Standard (append rápido),  
    " mas possui um índice Sorted secundário para buscas.  
    TYPES ty\_t\_materials TYPE STANDARD TABLE OF ty\_material  
                         WITH DEFAULT KEY  
                         WITH NON-UNIQUE SORTED KEY sk\_matnr COMPONENTS matnr.  
ENDCLASS.

CLASS zcl\_perf\_demo IMPLEMENTATION.  
  METHOD if\_oo\_adt\_classrun\~main.

    DATA: lt\_materials TYPE ty\_t\_materials.

    " 1\. Setup: Criar 500.000 registros para simular volume real  
    " O uso de VALUE com loops internos é rápido para mock data  
    out-\>write( 'Gerando 500.000 registros...' ).  
    DO 500000 TIMES.  
      INSERT VALUE \#(   
        matnr \= |M{ sy-index }|   
        desc  \= 'Material de Teste Performance'   
        price \= sy-index   
      ) INTO TABLE lt\_materials.  
    ENDDO.

    DATA(lv\_target) \= 'M499999'. " Buscando um item no final da tabela (pior caso linear)

    " 2\. Teste Lento: Busca Linear (Standard padrão)  
    " O sistema varre 499.999 linhas até achar.  
    GET RUN TIME FIELD DATA(t1).  
      
    LOOP AT lt\_materials INTO DATA(ls\_wa) WHERE matnr \= lv\_target.  
      " Achou e copiou  
    ENDLOOP.  
      
    GET RUN TIME FIELD DATA(t2).  
    DATA(diff\_linear) \= t2 \- t1.

    " 3\. Teste Rápido: Busca Binária (Chave Secundária)  
    " O sistema usa busca binária no índice secundário.  
    GET RUN TIME FIELD t1.  
      
    " Nota: Em LOOPs, devemos usar explicitamente USING KEY  
    LOOP AT lt\_materials ASSIGNING FIELD-SYMBOL(\<fs\_fast\>)   
                         USING KEY sk\_matnr   
                         WHERE matnr \= lv\_target.  
      " Achou via ponteiro  
    ENDLOOP.  
      
    GET RUN TIME FIELD t2.  
    DATA(diff\_sorted) \= t2 \- t1.

    " 4\. Exibição dos Resultados  
    out-\>write( |Tempo Busca Linear: { diff\_linear } microssegundos| ).  
    out-\>write( |Tempo Busca Sorted: { diff\_sorted } microssegundos| ).  
      
    IF diff\_sorted \> 0\.  
      out-\>write( |Ganho de Performance: { diff\_linear / diff\_sorted } vezes mais rápido| ).  
    ELSE.  
      out-\>write( 'A busca foi tão rápida que o timer não capturou precisão (\< 1ms).' ).  
    ENDIF.

  ENDMETHOD.  
ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Linear Search (Busca Sequencial):** O algoritmo mais simples e ineficiente para grandes volumes. Varrre a tabela do início ao fim. Complexidade **O(n)**. É o padrão para tabelas Standard sem chave explícita.  
* **Binary Search (Busca Binária):** Algoritmo de "dividir para conquistar". Divide a tabela ordenada ao meio repetidamente. Complexidade **O(log n)**. Usado em tabelas Sorted ou Secondary Sorted Keys. É extremamente escalável (1 milhão de registros requerem apenas \~20 comparações).  
* **Hash Algorithm (O(1)):** O "Santo Graal" da performance. O sistema calcula matematicamente o endereço de memória da linha baseado na chave. O tempo de acesso é constante, seja para 10 ou 10 milhões de linhas.  
* **Secondary Key:** Um índice adicional mantido pelo ABAP Runtime que permite buscas rápidas por campos que não fazem parte da chave primária ou da ordenação padrão da tabela. Tem um custo de memória (cerca de 10-15% a mais) e um custo leve de CPU na inserção, mas compensa enormemente na leitura.  
* **Field-Symbol:** Uma variável ponteiro (\<fs\>) que não armazena dados, mas aponta para um endereço de memória de outra variável. Essencial para loops de alta performance e modificação direta de tabelas internas.

#### **Guia de Decisão: Estratégias de Loop**

| Cenário | Estratégia Recomendada | Por quê? |
| :---- | :---- | :---- |
| **Ler um campo pequeno (Inteiro/Char)** | INTO DATA(wa) | Mais legível, custo de cópia desprezível. |
| **Modificar a tabela** | ASSIGNING FIELD-SYMBOL(\<fs\>) | Permite alterar \<fs\>-campo diretamente, refletindo na tabela. |
| **Tabela Larga (Muitas colunas)** | ASSIGNING FIELD-SYMBOL(\<fs\>) | Evita a alocação de memória duplicada e o custo de cópia de bytes. |
| **Filtrar por campo não-chave** | USING KEY Secundária | Transforma busca O(n) em O(log n) ou O(1). |

### **📝 Quiz de Fixação**

Q1: Por que a busca em uma Tabela Hashed é considerada a mais rápida para grandes volumes de dados (teoricamente O(1))?  
R: Porque ela utiliza um algoritmo de hash para transformar o valor da chave diretamente em um endereço de memória. Isso elimina a necessidade de varrer a tabela ou navegar em árvores de busca, permitindo acesso direto e instantâneo ao registro, independentemente do tamanho total da tabela.  
Q2: Tenho uma tabela Standard com 500.000 registros de Clientes e preciso buscar um cliente pelo CPF (que não é a chave primária) dentro de um loop crítico. O que devo fazer?  
R: Você deve definir uma Chave Secundária (WITH NON-UNIQUE SORTED KEY ou HASHED KEY) na definição da tabela utilizando o campo CPF. Em seguida, deve adaptar seu comando de leitura (READ ou LOOP) para usar explicitamente USING KEY nome\_da\_chave. Isso evitará o table scan linear.  
Q3: Qual a vantagem de performance de usar ASSIGNING FIELD-SYMBOL em vez de INTO em um comando LOOP sobre uma tabela com estrutura larga (ex: 200 colunas)?  
R: O FIELD-SYMBOL cria apenas uma referência (ponteiro) para a linha que já existe na memória da tabela. O INTO força o sistema a alocar um novo bloco de memória e copiar os dados de todas as 200 colunas para essa nova variável a cada iteração. Evitar essa cópia economiza tempo de CPU (alocação/cópia) e reduz a pressão sobre a memória.