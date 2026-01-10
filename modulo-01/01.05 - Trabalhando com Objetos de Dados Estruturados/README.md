# **Módulo 01: Programação ABAP Básica**

## **Aula 05: Trabalhando com Objetos de Dados Estruturados**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar com clareza **Tipos Elementares** de **Tipos Estruturados**, e compreender a distinção entre **Estruturas Planas (Flat)** e **Estruturas Profundas (Deep)**.  
2. Definir e declarar estruturas localmente utilizando TYPES e DATA, bem como reutilizar tipos globais do Dicionário de Dados (SE11).  
3. Acessar e manipular componentes individuais de uma estrutura utilizando a sintaxe do hífen (-).  
4. Dominar o operador construtor **CORRESPONDING**, incluindo suas variações avançadas (MAPPING, EXCEPT, BASE), essencial para a transferência de dados entre camadas no modelo RAP.

### **1. O que é uma Estrutura?**

Até este ponto do curso, manipulamos variáveis que armazenam um único valor por vez (tipos elementares), como um número inteiro (i) ou uma string de texto (string). No entanto, no mundo real dos negócios, os dados raramente andam sozinhos. Um "Cliente" não é apenas um ID; ele é um conjunto composto por Nome, Endereço, Telefone e Limite de Crédito.

Uma **Estrutura** é a representação técnica desse agrupamento lógico. É uma área de memória contínua dividida em subáreas chamadas **componentes**.

* **Analogia:** Se uma variável elementar é uma "célula" de Excel, uma estrutura é uma "linha" inteira dessa planilha.  
* **Importância no RAP:** No desenvolvimento ABAP moderno, estruturas são a base para a definição de Interfaces de BAdIs, assinaturas de métodos e, principalmente, para representar as entidades de negócio (Business Objects) antes de serem persistidas no banco.

### **2. Definindo e Declarando Estruturas**

A criação de estruturas segue o princípio de separar a "Definição do Molde" (Type) da "Criação do Objeto" (Data).

#### **Definindo o "Molde" (TYPES)**

Usamos o bloco BEGIN OF ... END OF para desenhar o layout da estrutura. Isso não aloca memória no sistema; apenas ensina ao compilador como os dados devem ser organizados.

Podemos definir estruturas baseadas em tipos elementares, tipos de dados globais (Data Elements) ou até misturar ambos.

" Definição Local (Válida apenas neste programa/classe)  
TYPES: BEGIN OF ty_flight_info,  
         airline_code TYPE /dmo/carrier_id,    " Elemento de Dados Global  
         flight_num   TYPE /dmo/connection_id, " Elemento de Dados Global  
         price        TYPE /dmo/flight_price,  " Elemento de Dados Global  
         currency     TYPE /dmo/currency_code, " Elemento de Dados Global  
         is_cancelled TYPE abap_bool,          " Tipo Embutido  
         " Campos técnicos podem ser adicionados livremente  
         _timestamp   TYPE timestampl,  
       END OF ty_flight_info.

#### **Criando a "Instância" (DATA)**

Com o molde definido, usamos o comando DATA para alocar espaço na memória RAM para guardar os valores.

" Criação da variável baseada no tipo local definido acima  
DATA: ls_flight TYPE ty_flight_info.

" Também é possível criar estruturas baseadas em tabelas do banco de dados (Global)  
" A estrutura ls_carrier terá exatamente as mesmas colunas da tabela /dmo/carrier  
DATA: ls_carrier TYPE /dmo/carrier.

" Acesso aos componentes usa o hífen (-)  
ls_flight-airline_code = 'AA'.  
ls_flight-flight_num   = '0017'.  
ls_flight-price        = '500.00'.  
ls_flight-is_cancelled = abap_false.

*Dica de Nomenclatura:* É uma convenção forte no ABAP usar o prefixo ls_ (*Local Structure*) ou wa_ (*Work Area*) para variáveis de estrutura, e ty_ para definições de tipos. Isso ajuda a identificar rapidamente se estamos lidando com dados ou definições.

### **3. O Operador CORRESPONDING (Fundamental para RAP)**

No desenvolvimento RAP, estamos constantemente movendo dados entre camadas: da camada de banco de dados para a camada de comportamento (BDEF), e desta para a projeção de consumo (CDS Projection). Essas camadas frequentemente têm estruturas muito parecidas, mas não idênticas.

Copiar campo por campo (ls_b-campo1 = ls_a-campo1) é trabalhoso e propenso a erros. O operador CORRESPONDING resolve isso inteligentemente.

#### **Sintaxe Básica e Evolução**

* Move-Corresponding (Antigo/Legado):  
  MOVE-CORRESPONDING ls_a TO ls_b.  
  Limitação: Não limpa os campos de ls_b que não existem em ls_a, e não permite regras complexas.  
* Corresponding Operator (Moderno - 7.40+):  
  ls_b = CORRESPONDING #( ls_a ).  
  Comportamento: Cria uma nova estrutura. Campos com nomes idênticos são copiados. Campos em ls_b que não existem em ls_a são inicializados (limpos), a menos que se use BASE.

#### **Recursos Avançados do CORRESPONDING**

1. **MAPPING (De/Para):** Usado quando os nomes dos campos são diferentes (ex: kunnr no banco vs customer_id na UI).  
2. **EXCEPT (Exclusão):** Usado para proteger campos sensíveis ou técnicos de serem sobrescritos (ex: não queremos copiar o ID da chave primária numa operação de atualização).  
3. **BASE (Preservação):** Essencial para atualizações (UPDATE). Ele pega a estrutura existente como base e aplica as mudanças por cima, preservando os campos que não foram tocados.

" Exemplo Teórico:  
ls_destino = CORRESPONDING #(   
    BASE ( ls_destino )  " Mantém os valores atuais de ls_destino  
    ls_origem            " Sobrescreve com valores de ls_origem onde nomes coincidem  
    MAPPING novonome = nomeantigo   
    EXCEPT campo_proibido   
).

### **4. Tipos de Estruturas: Planas vs. Profundas**

É crucial entender a "profundidade" da sua estrutura, pois isso afeta como ela é copiada e processada.

* **Flat Structure (Estrutura Plana):** Contém apenas tipos elementares ou outras estruturas planas. Tem tamanho fixo na memória. É o tipo padrão de uma linha de tabela de banco de dados.  
* **Deep Structure (Estrutura Profunda):** Contém pelo menos um componente que é dinâmico (referência), como uma **String**, uma **Tabela Interna** ou uma **Referência de Objeto**.  
  * *Exemplo:* Uma estrutura de "Pedido de Venda" que contém um campo "Itens", onde "Itens" é uma tabela interna com lista de produtos.  
  * *Atenção:* Operações com estruturas profundas exigem cuidado extra, pois envolvem ponteiros de memória. O CORRESPONDING simples faz cópia rasa (shallow copy) das referências, a menos que se use a variante DEEP.

TYPES: BEGIN OF ty_passenger,  
         name    TYPE string,       " String torna a estrutura profunda (Deep)  
         address TYPE ty_address,   " Outra estrutura (Nested)  
         flights TYPE tt_flights,   " Tabela Interna dentro da estrutura  
       END OF ty_passenger.

" Acesso aninhado:  
ls_passenger-address-city = 'New York'.  
" Acesso a tabela interna dentro da estrutura:  
ls_passenger-flights[ 1 ]-price = '200.00'.

### **5. Exemplo Prático: Manipulação, Mapeamento e BASE**

Neste exemplo expandido, simulamos um cenário comum no RAP: ler dados do banco, preparar para a UI, e depois simular uma atualização parcial vinda da UI preservando dados originais.

CLASS zcl_structures_demo DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if_oo_adt_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl_structures_demo IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " ---------------------------------------------------------------------  
    " 1. Definições de Tipos (Simulando Camadas do RAP)  
    " ---------------------------------------------------------------------  
      
    " Estrutura de Banco de Dados (Nomes técnicos, campos de sistema)  
    TYPES: BEGIN OF ty_db_flight,  
             carrier_id    TYPE string,  
             connection_id TYPE string,  
             flight_date   TYPE d,  
             price         TYPE p LENGTH 10 DECIMALS 2,  
             currency_code TYPE string,  
             created_by    TYPE string, " Campo de auditoria  
             created_at    TYPE timestampl,  
           END OF ty_db_flight.

    " Estrutura de UI/Consumo (Nomes amigáveis, sem campos de sistema)  
    TYPES: BEGIN OF ty_ui_flight,  
             airline       TYPE string, " Mapeado de carrier_id  
             connection    TYPE string, " Mapeado de connection_id  
             price         TYPE p LENGTH 10 DECIMALS 2,  
             currency      TYPE string,  
             status_text   TYPE string, " Campo apenas de UI  
           END OF ty_ui_flight.

    " ---------------------------------------------------------------------  
    " 2. Cenário A: Leitura (DB -> UI)  
    " ---------------------------------------------------------------------  
      
    " Populando a origem (Mock de um SELECT)  
    DATA(ls_db_source) = VALUE ty_db_flight(  
      carrier_id    = 'LH'  
      connection_id = '0400'  
      price         = '1250.50'  
      currency_code = 'EUR'  
      created_by    = 'USER_SAP'  
      created_at    = '20230101120000'  
    ).

    " Usando CORRESPONDING com MAPPING para traduzir técnico -> amigável  
    " Note que created_by e created_at são ignorados pois não existem no destino  
    DATA(ls_ui_target) = CORRESPONDING ty_ui_flight(  
      ls_db_source  
      MAPPING airline    = carrier_id  
              connection = connection_id  
              currency   = currency_code  
    ).  
      
    ls_ui_target-status_text = 'Confirmado'. " Enriquecendo dados na UI

    out->write( '--- Cenário A: DB para UI ---' ).  
    out->write( |DB: { ls_db_source-carrier_id } / { ls_db_source-price }| ).  
    out->write( |UI: { ls_ui_target-airline } / { ls_ui_target-price } ({ ls_ui_target-status_text })| ).

    " ---------------------------------------------------------------------  
    " 2. Cenário B: Atualização Parcial (UI -> DB) usando BASE e EXCEPT  
    " ---------------------------------------------------------------------  
      
    " Imagine que a UI mandou uma alteração: Preço mudou, mas airline e connection são chaves (não mudam)  
    " E a UI não sabe nada sobre 'created_by'.  
      
    DATA(ls_ui_input) = ls_ui_target.  
    ls_ui_input-price = '999.00'. " Novo preço vindo da tela

    " Queremos atualizar ls_db_source com os dados de ls_ui_input  
    " MAS:  
    " 1. Devemos preservar o 'created_by' original (usando BASE)  
    " 2. Não queremos alterar chaves sem querer (usando EXCEPT, se aplicável) ou MAPPING reverso  
      
    DATA(ls_db_updated) = CORRESPONDING ty_db_flight(  
      BASE ( ls_db_source )  " Começa com a cópia dos dados atuais do banco  
      ls_ui_input            " Aplica as mudanças vindas da UI  
      MAPPING carrier_id    = airline     " Mapeamento reverso  
              connection_id = connection  
              currency_code = currency  
      EXCEPT carrier_id      " Exemplo: Vamos proteger a chave carrier_id de ser sobrescrita   
             connection_id   " mesmo se viesse diferente da UI  
    ).

    out->write( ' ' ).  
    out->write( '--- Cenário B: UI para DB (Update com BASE) ---' ).  
    out->write( |Original DB Price: { ls_db_source-price } | ).  
    out->write( |Updated  DB Price: { ls_db_updated-price } (Preço Atualizado)| ).  
    out->write( |Original CreatedBy: { ls_db_source-created_by }| ).  
    out->write( |Updated  CreatedBy: { ls_db_updated-created_by } (Preservado graças ao BASE)| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Structure (Estrutura):** Um objeto de dados complexo que contém uma sequência de componentes (campos) de qualquer tipo. É a representação em memória de uma linha de tabela ou entidade de negócio.  
* **Flat Structure (Estrutura Plana):** Estrutura que contém apenas tipos elementares de comprimento fixo. Não contém strings, tabelas internas ou referências. Essencial para chaves de tabelas e operações de banco de dados simples.  
* **Deep Structure (Estrutura Profunda):** Estrutura que contém referências (strings, tabelas internas, objetos). Exige gerenciamento de memória mais complexo pelo sistema.  
* **Nested Structure (Estrutura Aninhada):** Uma estrutura que contém outra subestrutura como um dos seus componentes. Permite modelar dados hierárquicos (ex: Cabeçalho -> Endereço).  
* **CORRESPONDING Operator:** Poderoso operador construtor que projeta dados de uma estrutura para outra baseando-se na correspondência de nomes ou regras de mapeamento explícito.  
* **BASE Addition:** Cláusula do operador CORRESPONDING (e outros construtores) que permite definir um valor inicial para a estrutura de destino antes de aplicar a cópia dos novos valores, essencial para operações de *Merge* ou *Update*.  
* **TYPES vs DATA:** TYPES define o "molde" ou a "planta" da estrutura (tempo de compilação). DATA aloca a memória e cria a "instância" utilizável (tempo de execução).

#### **Pontos de Atenção (Sintaxe Legada vs. Moderna)**

| Conceito | ABAP Legado (Evitar) | ABAP Moderno (Recomendado) |
| :---- | :---- | :---- |
| Transferência por Nome | MOVE-CORRESPONDING a TO b. | b = CORRESPONDING #( a ). |
| Preservar Dados | Lógica manual de IFs para não limpar | b = CORRESPONDING #( BASE ( b ) a ). |
| Inicialização de Valores | Declaração linha a linha | DATA(s) = VALUE tipo( cmp1 = 'A' cmp2 = 'B' ). |
| Tipos Locais | TYPES: BEGIN OF ... (igual) | Uso intensivo de tipos inline em Classes |
| Definição de Work Area | DATA: wa LIKE ztabela. | DATA: wa TYPE ztabela. |

### **📝 Quiz de Fixação**

Q1: Qual é o símbolo utilizado para acessar um componente dentro de uma estrutura ABAP?  
R: O hífen (-). Exemplo: ls_usuario-nome. Isso difere da maioria das linguagens C-like que usam o ponto (.), que no ABAP é o terminador de instrução.  
Q2: Se eu usar CORRESPONDING entre duas estruturas que têm campos com nomes totalmente diferentes, o que acontece?  
R: Por padrão, nada será copiado e os campos da estrutura de destino ficarão vazios (inicializados), pois o operador busca nomes idênticos. Para que a cópia ocorra, é necessário usar a cláusula MAPPING (ex: MAPPING destino = origem) para ensinar ao sistema como ligar os campos.  
Q3: Qual a diferença crucial entre ls_b = CORRESPONDING #( ls_a ) e ls_b = CORRESPONDING #( BASE ( ls_b ) ls_a )?  
R: A primeira instrução limpa completamente ls_b antes de copiar os dados correspondentes de ls_a (os campos sem par em ls_a ficarão vazios). A segunda instrução mantém os valores originais de ls_b e apenas atualiza (sobrescreve) os campos que coincidirem com ls_a, agindo como um "Merge" de dados.  
Q4: O que caracteriza uma "Estrutura Profunda" (Deep Structure) e por que devemos ter cuidado com elas?  
R: Uma estrutura profunda contém campos de tamanho dinâmico ou referências, como STRING, TABLE ou REF TO. Devemos ter cuidado porque operações simples de cópia podem envolver apenas a referência (endereço de memória) e não o valor real, além de não poderem ser usadas em certos contextos de banco de dados ou chaves de tabelas simples.