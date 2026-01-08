# **Módulo 01: Programação ABAP Básica**

## **Aula 05: Trabalhando com Objetos de Dados Estruturados**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar com clareza **Tipos Elementares** de **Tipos Estruturados**, e compreender a distinção entre **Estruturas Planas (Flat)** e **Estruturas Profundas (Deep)**.  
2. Definir e declarar estruturas localmente utilizando TYPES e DATA, bem como reutilizar tipos globais do Dicionário de Dados (SE11).  
3. Acessar e manipular componentes individuais de uma estrutura utilizando a sintaxe do hífen (-).  
4. Dominar o operador construtor **CORRESPONDING**, incluindo suas variações avançadas (MAPPING, EXCEPT, BASE), essencial para a transferência de dados entre camadas no modelo RAP.

### **1\. O que é uma Estrutura?**

Até este ponto do curso, manipulamos variáveis que armazenam um único valor por vez (tipos elementares), como um número inteiro (i) ou uma string de texto (string). No entanto, no mundo real dos negócios, os dados raramente andam sozinhos. Um "Cliente" não é apenas um ID; ele é um conjunto composto por Nome, Endereço, Telefone e Limite de Crédito.

Uma **Estrutura** é a representação técnica desse agrupamento lógico. É uma área de memória contínua dividida em subáreas chamadas **componentes**.

* **Analogia:** Se uma variável elementar é uma "célula" de Excel, uma estrutura é uma "linha" inteira dessa planilha.  
* **Importância no RAP:** No desenvolvimento ABAP moderno, estruturas são a base para a definição de Interfaces de BAdIs, assinaturas de métodos e, principalmente, para representar as entidades de negócio (Business Objects) antes de serem persistidas no banco.

### **2\. Definindo e Declarando Estruturas**

A criação de estruturas segue o princípio de separar a "Definição do Molde" (Type) da "Criação do Objeto" (Data).

#### **Definindo o "Molde" (TYPES)**

Usamos o bloco BEGIN OF ... END OF para desenhar o layout da estrutura. Isso não aloca memória no sistema; apenas ensina ao compilador como os dados devem ser organizados.

Podemos definir estruturas baseadas em tipos elementares, tipos de dados globais (Data Elements) ou até misturar ambos.

" Definição Local (Válida apenas neste programa/classe)  
TYPES: BEGIN OF ty\_flight\_info,  
         airline\_code TYPE /dmo/carrier\_id,    " Elemento de Dados Global  
         flight\_num   TYPE /dmo/connection\_id, " Elemento de Dados Global  
         price        TYPE /dmo/flight\_price,  " Elemento de Dados Global  
         currency     TYPE /dmo/currency\_code, " Elemento de Dados Global  
         is\_cancelled TYPE abap\_bool,          " Tipo Embutido  
         " Campos técnicos podem ser adicionados livremente  
         \_timestamp   TYPE timestampl,  
       END OF ty\_flight\_info.

#### **Criando a "Instância" (DATA)**

Com o molde definido, usamos o comando DATA para alocar espaço na memória RAM para guardar os valores.

" Criação da variável baseada no tipo local definido acima  
DATA: ls\_flight TYPE ty\_flight\_info.

" Também é possível criar estruturas baseadas em tabelas do banco de dados (Global)  
" A estrutura ls\_carrier terá exatamente as mesmas colunas da tabela /dmo/carrier  
DATA: ls\_carrier TYPE /dmo/carrier.

" Acesso aos componentes usa o hífen (-)  
ls\_flight-airline\_code \= 'AA'.  
ls\_flight-flight\_num   \= '0017'.  
ls\_flight-price        \= '500.00'.  
ls\_flight-is\_cancelled \= abap\_false.

*Dica de Nomenclatura:* É uma convenção forte no ABAP usar o prefixo ls\_ (*Local Structure*) ou wa\_ (*Work Area*) para variáveis de estrutura, e ty\_ para definições de tipos. Isso ajuda a identificar rapidamente se estamos lidando com dados ou definições.

### **3\. O Operador CORRESPONDING (Fundamental para RAP)**

No desenvolvimento RAP, estamos constantemente movendo dados entre camadas: da camada de banco de dados para a camada de comportamento (BDEF), e desta para a projeção de consumo (CDS Projection). Essas camadas frequentemente têm estruturas muito parecidas, mas não idênticas.

Copiar campo por campo (ls\_b-campo1 \= ls\_a-campo1) é trabalhoso e propenso a erros. O operador CORRESPONDING resolve isso inteligentemente.

#### **Sintaxe Básica e Evolução**

* Move-Corresponding (Antigo/Legado):  
  MOVE-CORRESPONDING ls\_a TO ls\_b.  
  Limitação: Não limpa os campos de ls\_b que não existem em ls\_a, e não permite regras complexas.  
* Corresponding Operator (Moderno \- 7.40+):  
  ls\_b \= CORRESPONDING \#( ls\_a ).  
  Comportamento: Cria uma nova estrutura. Campos com nomes idênticos são copiados. Campos em ls\_b que não existem em ls\_a são inicializados (limpos), a menos que se use BASE.

#### **Recursos Avançados do CORRESPONDING**

1. **MAPPING (De/Para):** Usado quando os nomes dos campos são diferentes (ex: kunnr no banco vs customer\_id na UI).  
2. **EXCEPT (Exclusão):** Usado para proteger campos sensíveis ou técnicos de serem sobrescritos (ex: não queremos copiar o ID da chave primária numa operação de atualização).  
3. **BASE (Preservação):** Essencial para atualizações (UPDATE). Ele pega a estrutura existente como base e aplica as mudanças por cima, preservando os campos que não foram tocados.

" Exemplo Teórico:  
ls\_destino \= CORRESPONDING \#(   
    BASE ( ls\_destino )  " Mantém os valores atuais de ls\_destino  
    ls\_origem            " Sobrescreve com valores de ls\_origem onde nomes coincidem  
    MAPPING novonome \= nomeantigo   
    EXCEPT campo\_proibido   
).

### **4\. Tipos de Estruturas: Planas vs. Profundas**

É crucial entender a "profundidade" da sua estrutura, pois isso afeta como ela é copiada e processada.

* **Flat Structure (Estrutura Plana):** Contém apenas tipos elementares ou outras estruturas planas. Tem tamanho fixo na memória. É o tipo padrão de uma linha de tabela de banco de dados.  
* **Deep Structure (Estrutura Profunda):** Contém pelo menos um componente que é dinâmico (referência), como uma **String**, uma **Tabela Interna** ou uma **Referência de Objeto**.  
  * *Exemplo:* Uma estrutura de "Pedido de Venda" que contém um campo "Itens", onde "Itens" é uma tabela interna com lista de produtos.  
  * *Atenção:* Operações com estruturas profundas exigem cuidado extra, pois envolvem ponteiros de memória. O CORRESPONDING simples faz cópia rasa (shallow copy) das referências, a menos que se use a variante DEEP.

TYPES: BEGIN OF ty\_passenger,  
         name    TYPE string,       " String torna a estrutura profunda (Deep)  
         address TYPE ty\_address,   " Outra estrutura (Nested)  
         flights TYPE tt\_flights,   " Tabela Interna dentro da estrutura  
       END OF ty\_passenger.

" Acesso aninhado:  
ls\_passenger-address-city \= 'New York'.  
" Acesso a tabela interna dentro da estrutura:  
ls\_passenger-flights\[ 1 \]-price \= '200.00'.

### **5\. Exemplo Prático: Manipulação, Mapeamento e BASE**

Neste exemplo expandido, simulamos um cenário comum no RAP: ler dados do banco, preparar para a UI, e depois simular uma atualização parcial vinda da UI preservando dados originais.

CLASS zcl\_structures\_demo DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_structures\_demo IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " \---------------------------------------------------------------------  
    " 1\. Definições de Tipos (Simulando Camadas do RAP)  
    " \---------------------------------------------------------------------  
      
    " Estrutura de Banco de Dados (Nomes técnicos, campos de sistema)  
    TYPES: BEGIN OF ty\_db\_flight,  
             carrier\_id    TYPE string,  
             connection\_id TYPE string,  
             flight\_date   TYPE d,  
             price         TYPE p LENGTH 10 DECIMALS 2,  
             currency\_code TYPE string,  
             created\_by    TYPE string, " Campo de auditoria  
             created\_at    TYPE timestampl,  
           END OF ty\_db\_flight.

    " Estrutura de UI/Consumo (Nomes amigáveis, sem campos de sistema)  
    TYPES: BEGIN OF ty\_ui\_flight,  
             airline       TYPE string, " Mapeado de carrier\_id  
             connection    TYPE string, " Mapeado de connection\_id  
             price         TYPE p LENGTH 10 DECIMALS 2,  
             currency      TYPE string,  
             status\_text   TYPE string, " Campo apenas de UI  
           END OF ty\_ui\_flight.

    " \---------------------------------------------------------------------  
    " 2\. Cenário A: Leitura (DB \-\> UI)  
    " \---------------------------------------------------------------------  
      
    " Populando a origem (Mock de um SELECT)  
    DATA(ls\_db\_source) \= VALUE ty\_db\_flight(  
      carrier\_id    \= 'LH'  
      connection\_id \= '0400'  
      price         \= '1250.50'  
      currency\_code \= 'EUR'  
      created\_by    \= 'USER\_SAP'  
      created\_at    \= '20230101120000'  
    ).

    " Usando CORRESPONDING com MAPPING para traduzir técnico \-\> amigável  
    " Note que created\_by e created\_at são ignorados pois não existem no destino  
    DATA(ls\_ui\_target) \= CORRESPONDING ty\_ui\_flight(  
      ls\_db\_source  
      MAPPING airline    \= carrier\_id  
              connection \= connection\_id  
              currency   \= currency\_code  
    ).  
      
    ls\_ui\_target-status\_text \= 'Confirmado'. " Enriquecendo dados na UI

    out-\>write( '--- Cenário A: DB para UI \---' ).  
    out-\>write( |DB: { ls\_db\_source-carrier\_id } / { ls\_db\_source-price }| ).  
    out-\>write( |UI: { ls\_ui\_target-airline } / { ls\_ui\_target-price } ({ ls\_ui\_target-status\_text })| ).

    " \---------------------------------------------------------------------  
    " 2\. Cenário B: Atualização Parcial (UI \-\> DB) usando BASE e EXCEPT  
    " \---------------------------------------------------------------------  
      
    " Imagine que a UI mandou uma alteração: Preço mudou, mas airline e connection são chaves (não mudam)  
    " E a UI não sabe nada sobre 'created\_by'.  
      
    DATA(ls\_ui\_input) \= ls\_ui\_target.  
    ls\_ui\_input-price \= '999.00'. " Novo preço vindo da tela

    " Queremos atualizar ls\_db\_source com os dados de ls\_ui\_input  
    " MAS:  
    " 1\. Devemos preservar o 'created\_by' original (usando BASE)  
    " 2\. Não queremos alterar chaves sem querer (usando EXCEPT, se aplicável) ou MAPPING reverso  
      
    DATA(ls\_db\_updated) \= CORRESPONDING ty\_db\_flight(  
      BASE ( ls\_db\_source )  " Começa com a cópia dos dados atuais do banco  
      ls\_ui\_input            " Aplica as mudanças vindas da UI  
      MAPPING carrier\_id    \= airline     " Mapeamento reverso  
              connection\_id \= connection  
              currency\_code \= currency  
      EXCEPT carrier\_id      " Exemplo: Vamos proteger a chave carrier\_id de ser sobrescrita   
             connection\_id   " mesmo se viesse diferente da UI  
    ).

    out-\>write( ' ' ).  
    out-\>write( '--- Cenário B: UI para DB (Update com BASE) \---' ).  
    out-\>write( |Original DB Price: { ls\_db\_source-price } | ).  
    out-\>write( |Updated  DB Price: { ls\_db\_updated-price } (Preço Atualizado)| ).  
    out-\>write( |Original CreatedBy: { ls\_db\_source-created\_by }| ).  
    out-\>write( |Updated  CreatedBy: { ls\_db\_updated-created\_by } (Preservado graças ao BASE)| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Structure (Estrutura):** Um objeto de dados complexo que contém uma sequência de componentes (campos) de qualquer tipo. É a representação em memória de uma linha de tabela ou entidade de negócio.  
* **Flat Structure (Estrutura Plana):** Estrutura que contém apenas tipos elementares de comprimento fixo. Não contém strings, tabelas internas ou referências. Essencial para chaves de tabelas e operações de banco de dados simples.  
* **Deep Structure (Estrutura Profunda):** Estrutura que contém referências (strings, tabelas internas, objetos). Exige gerenciamento de memória mais complexo pelo sistema.  
* **Nested Structure (Estrutura Aninhada):** Uma estrutura que contém outra subestrutura como um dos seus componentes. Permite modelar dados hierárquicos (ex: Cabeçalho \-\> Endereço).  
* **CORRESPONDING Operator:** Poderoso operador construtor que projeta dados de uma estrutura para outra baseando-se na correspondência de nomes ou regras de mapeamento explícito.  
* **BASE Addition:** Cláusula do operador CORRESPONDING (e outros construtores) que permite definir um valor inicial para a estrutura de destino antes de aplicar a cópia dos novos valores, essencial para operações de *Merge* ou *Update*.  
* **TYPES vs DATA:** TYPES define o "molde" ou a "planta" da estrutura (tempo de compilação). DATA aloca a memória e cria a "instância" utilizável (tempo de execução).

#### **Pontos de Atenção (Sintaxe Legada vs. Moderna)**

| Conceito | ABAP Legado (Evitar) | ABAP Moderno (Recomendado) |
| :---- | :---- | :---- |
| Transferência por Nome | MOVE-CORRESPONDING a TO b. | b \= CORRESPONDING \#( a ). |
| Preservar Dados | Lógica manual de IFs para não limpar | b \= CORRESPONDING \#( BASE ( b ) a ). |
| Inicialização de Valores | Declaração linha a linha | DATA(s) \= VALUE tipo( cmp1 \= 'A' cmp2 \= 'B' ). |
| Tipos Locais | TYPES: BEGIN OF ... (igual) | Uso intensivo de tipos inline em Classes |
| Definição de Work Area | DATA: wa LIKE ztabela. | DATA: wa TYPE ztabela. |

### **📝 Quiz de Fixação**

Q1: Qual é o símbolo utilizado para acessar um componente dentro de uma estrutura ABAP?  
R: O hífen (-). Exemplo: ls\_usuario-nome. Isso difere da maioria das linguagens C-like que usam o ponto (.), que no ABAP é o terminador de instrução.  
Q2: Se eu usar CORRESPONDING entre duas estruturas que têm campos com nomes totalmente diferentes, o que acontece?  
R: Por padrão, nada será copiado e os campos da estrutura de destino ficarão vazios (inicializados), pois o operador busca nomes idênticos. Para que a cópia ocorra, é necessário usar a cláusula MAPPING (ex: MAPPING destino \= origem) para ensinar ao sistema como ligar os campos.  
Q3: Qual a diferença crucial entre ls\_b \= CORRESPONDING \#( ls\_a ) e ls\_b \= CORRESPONDING \#( BASE ( ls\_b ) ls\_a )?  
R: A primeira instrução limpa completamente ls\_b antes de copiar os dados correspondentes de ls\_a (os campos sem par em ls\_a ficarão vazios). A segunda instrução mantém os valores originais de ls\_b e apenas atualiza (sobrescreve) os campos que coincidirem com ls\_a, agindo como um "Merge" de dados.  
Q4: O que caracteriza uma "Estrutura Profunda" (Deep Structure) e por que devemos ter cuidado com elas?  
R: Uma estrutura profunda contém campos de tamanho dinâmico ou referências, como STRING, TABLE ou REF TO. Devemos ter cuidado porque operações simples de cópia podem envolver apenas a referência (endereço de memória) e não o valor real, além de não poderem ser usadas em certos contextos de banco de dados ou chaves de tabelas simples.