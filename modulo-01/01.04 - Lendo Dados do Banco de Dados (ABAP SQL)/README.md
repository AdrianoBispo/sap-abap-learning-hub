# **Módulo 01: Programação ABAP Básica**

## **Aula 04: Lendo Dados do Banco de Dados (ABAP SQL)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Escrever comandos **SELECT** robustos utilizando a sintaxe moderna (Strict Mode), compreendendo suas regras sintáticas e benefícios.  
2. Entender a fundo o uso do caractere de escape **@ (Host Variables)** para diferenciar variáveis ABAP de colunas SQL.  
3. Utilizar **Declarações Inline** (@DATA) para criação automática de tabelas internas e estruturas complexas baseadas na projeção da consulta.  
4. Aplicar filtros avançados (WHERE, IN, LIKE), ordenação (ORDER BY) e limitar resultados (UP TO n ROWS) de forma eficiente.  
5. Diferenciar o uso de **SELECT SINGLE** para leituras únicas e entender as variáveis de sistema **sy-subrc** e **sy-dbcnt**.

### **1\. ABAP SQL Moderno: O Que Mudou e Por Quê?**

Antigamente chamado de *Open SQL*, o ABAP SQL é a camada de abstração que traduz nosso código ABAP para a linguagem específica do banco de dados subjacente (seja ele HANA, Oracle, DB2, etc.).

No ABAP Clássico, o SQL era limitado. Trazíamos os dados brutos para o servidor de aplicação e processávamos tudo com loops LOOP AT. Com a chegada do SAP HANA e do **ABAP 7.40+**, o paradigma mudou para **Code Pushdown** (empurrar o código para o banco). Para suportar novas funcionalidades (como cálculos e expressões), a SAP introduziu uma nova sintaxe.

#### **O "Strict Mode" (Modo Estrito)**

O Modo Estrito não é uma configuração que você liga; ele é ativado automaticamente pelo compilador assim que você usa qualquer funcionalidade moderna (como vírgulas na lista de campos ou o símbolo @).

**Regras do Jogo:**

* **Vírgulas Obrigatórias:** Ao contrário do antigo (espaços), os campos *devem* ser separados por vírgula.  
* **INTO no Final:** A cláusula de destino deve vir após as cláusulas de seleção, filtro e ordenação.  
* **Escape @ Obrigatório:** Toda variável do programa ABAP usada na query deve ter o prefixo @.

### **2\. A Sintaxe do SELECT Moderno**

#### **Selecionando Múltiplas Linhas (Tabela Interna)**

Vamos analisar uma consulta na tabela /dmo/connection (Modelo de Voo).

* Estilo Antigo (Obsoleto no Cloud):  
  Exigia declaração prévia e a ordem das cláusulas era rígida.  
  DATA: lt\_connections TYPE TABLE OF /dmo/connection.  
  " Sintaxe antiga: sem vírgulas, sem @, INTO no meio  
  SELECT \* FROM /dmo/connection INTO TABLE lt\_connections  
           WHERE carrier\_id \= 'LH'.

* Estilo Moderno (Recomendado):  
  Permite renomear colunas (AS), usar literais e declarar o destino na hora.  
  DATA(lv\_airline) \= 'LH'.

  " A sintaxe moderna flui como uma frase lógica:  
  " DE ONDE \-\> O QUE \-\> COMO FILTRAR \-\> COMO ORDENAR \-\> PARA ONDE  
  SELECT FROM /dmo/connection  
    FIELDS   
      carrier\_id,   
      connection\_id,   
      airport\_from AS departure\_airport, " ALIAS: Renomeando coluna  
      airport\_to   AS arrival\_airport,   " ALIAS: Renomeando coluna  
      distance  
    WHERE carrier\_id \= @lv\_airline  
    ORDER BY carrier\_id, connection\_id DESCENDING " Ordenação mista  
    INTO TABLE @DATA(lt\_flights).

  *Nota:* A tabela lt\_flights terá os campos carrier\_id, connection\_id, departure\_airport, etc. O tipo é inferido automaticamente.

#### **Selecionando uma Única Linha (Estrutura)**

Existem duas formas principais de ler um único registro.

A. SELECT SINGLE (Leitura por Chave)  
Ideal quando você tem a chave primária completa e quer validar existência ou ler atributos.  
SELECT SINGLE FROM /dmo/connection  
  FIELDS airport\_from, airport\_to  
  WHERE carrier\_id \= 'AA'  
    AND connection\_id \= '0017'  
  INTO @DATA(ls\_flight\_info).

B. SELECT ... UP TO 1 ROWS (Amostragem)  
Ideal quando você não tem a chave completa e quer apenas "o primeiro que encontrar" ou "o mais recente" (se combinado com ORDER BY).  
SELECT FROM /dmo/connection  
  FIELDS connection\_id  
  WHERE airport\_from \= 'JFK'  
  ORDER BY distance ASCENDING " Pega o voo mais curto saindo de JFK  
  INTO TABLE @DATA(lt\_one\_flight)  
  UP TO 1 ROWS.

### **3\. O Porquê do "@" (Host Variables)**

No Strict Mode, o parser do SQL precisa de uma distinção clara entre o que pertence ao **Banco de Dados** e o que pertence ao **Programa ABAP**.

* carrier\_id: É uma coluna que existe na tabela do banco.  
* @lv\_airline: É uma variável ("Host Variable") hospedada na memória do programa ABAP.

Por que isso é seguro?  
O uso do @ ajuda a prevenir ambiguidades e melhora a segurança. O banco sabe exatamente que aquele valor é um parâmetro externo, o que ajuda na performance (reaproveitamento do plano de execução) e segurança.  
*Exemplo de Erro Comum:*

" ERRADO no Strict Mode  
SELECT FROM /dmo/flight FIELDS \* WHERE carrier\_id \= lv\_id INTO TABLE @data(lt\_tab).

" CORRETO  
SELECT FROM /dmo/flight FIELDS \* WHERE carrier\_id \= @lv\_id INTO TABLE @data(lt\_tab).

### **4\. Variáveis de Sistema: O Painel de Controle**

Após cada comando SELECT, o sistema atualiza duas variáveis críticas na estrutura sy:

1. **sy-subrc (Return Code):**  
   * 0: Sucesso. Pelo menos uma linha foi encontrada.  
   * 4: Aviso. Nenhuma linha foi encontrada que satisfaça o WHERE.  
   * 8: Erro (menos comum em SELECTs simples).  
   * *Dica:* Sempre verifique sy-subrc imediatamente após o SELECT.  
2. **sy-dbcnt (Database Count):**  
   * Contém o número de linhas processadas/retornadas.  
   * Se sy-subrc \= 0, sy-dbcnt dirá quantas linhas vieram (ex: 500).  
   * Se sy-subrc \= 4, sy-dbcnt será 0\.

### **5\. Exemplo Prático: Buscando e Exibindo Dados**

Neste exemplo expandido, vamos usar uma tabela de intervalos (Range) para filtro e analisar a contagem de registros.

CLASS zcl\_read\_data DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_read\_data IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " 1\. Preparação de Filtros  
    DATA(lv\_carrier) \= 'UA'.   
    DATA(lv\_min\_distance) \= 1000\.

    " 2\. SELECT com Expressões Lógicas e Projeção  
    " Queremos voos da UA com distância maior que 1000, ordenados pela maior distância  
    SELECT FROM /dmo/connection  
      FIELDS   
        carrier\_id,   
        connection\_id,   
        airport\_from,   
        airport\_to,   
        distance,   
        distance\_unit  
      WHERE carrier\_id \= @lv\_carrier  
        AND distance   \> @lv\_min\_distance  
      ORDER BY distance DESCENDING  
      INTO TABLE @DATA(lt\_results).

    " 3\. Análise do Resultado  
    IF sy-subrc \= 0\.  
      " Sucesso: Mostra quantos registros achou usando sy-dbcnt  
      out-\>write( |Sucesso\! Foram encontrados { sy-dbcnt } voos longos da { lv\_carrier }.| ).  
      out-\>write( '--------------------------------------------------' ).

      LOOP AT lt\_results INTO DATA(ls\_flight).  
        " Exibe detalhes  
        out-\>write( |Voo { ls\_flight-connection\_id }: { ls\_flight-distance } { ls\_flight-distance\_unit } | &&  
                    |({ ls\_flight-airport\_from } \-\> { ls\_flight-airport\_to })| ).  
      ENDLOOP.  
        
    ELSE.  
      " Falha  
      out-\>write( |Nenhum voo encontrado com distância maior que { lv\_min\_distance }.| ).  
    ENDIF.

    " 4\. Exemplo de SELECT SINGLE  
    " Verificando se existe algum voo específico  
    SELECT SINGLE FROM /dmo/connection  
      FIELDS @abap\_true  
      WHERE carrier\_id \= 'AA'  
        AND connection\_id \= '0017'  
      INTO @DATA(lv\_exists).

    IF lv\_exists \= abap\_true.  
      out-\>write( 'O voo AA 0017 existe no banco de dados.' ).  
    ENDIF.

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **ABAP SQL (Antigo Open SQL):** Subconjunto da linguagem ABAP usado para acessar o banco de dados de forma independente da plataforma. Ele converte a sintaxe ABAP para o SQL nativo do banco (HANA, Oracle, etc.).  
* **Host Variable (@):** Uma variável declarada no programa ABAP utilizada dentro de uma instrução SQL. O prefixo @ é obrigatório no modo estrito para diferenciação de colunas.  
* **Code Pushdown:** Paradigma de performance onde movemos a lógica de processamento de dados (cálculos, agregações, filtros complexos) da camada de aplicação (ABAP) para a camada de banco de dados (HANA), trafegando apenas o resultado final.  
* **SELECT SINGLE:** Instrução otimizada para buscar **uma única linha**. Se a chave primária completa não for fornecida, retorna a primeira linha arbitrária encontrada.  
* **sy-subrc:** Variável de sistema que indica o status da execução. 0 \= Sucesso, 4 \= Dados não encontrados.  
* **sy-dbcnt:** Variável de sistema que armazena a **quantidade de linhas** afetadas ou lidas pela última instrução SQL.  
* **FIELDS:** Palavra-chave introduzida no ABAP 7.40+ para listar as colunas a serem lidas. Embora opcional em alguns contextos, é recomendada para clareza e obrigatória em queries complexas.  
* **Inline Declaration (@DATA):** Recurso que permite criar a tabela ou estrutura de destino no momento exato da leitura, inferindo os tipos de dados a partir da lista de campos do SELECT.

#### **Pontos de Atenção (Performance e Sintaxe)**

| Conceito | Prática Ruim (Legado) | Prática Boa (Moderno/Strict) | Motivo |
| :---- | :---- | :---- | :---- |
| **Seleção de Colunas** | SELECT \* | SELECT field1, field2 | Economiza memória e tráfego de rede. |
| **Verificar Existência** | SELECT \* ... UP TO 1 ROWS | SELECT SINGLE @abap\_true ... | Mais leve, não transporta dados inúteis. |
| **Escape de Variável** | WHERE id \= lv\_id | WHERE id \= @lv\_id | Clareza para o parser e segurança. |
| **Looping** | SELECT dentro de LOOP | FOR ALL ENTRIES ou JOIN | Evita "N+1 selects" (múltiplas idas ao banco). |
| **Destino** | INTO CORRESPONDING FIELDS | Lista explícita com INTO TABLE | CORRESPONDING é mais lento pois compara nomes. |

### **📝 Quiz de Fixação**

Q1: O que acontece se eu esquecer de colocar o símbolo @ antes de uma variável ABAP dentro de um SELECT moderno?  
R: Ocorrerá um erro de sintaxe. No modo estrito (Strict Mode), ativado pelo uso de vírgulas ou declarações inline, o compilador exige o @ para diferenciar inequivocamente o que é coluna do banco de dados e o que é variável do programa (Host Variable).  
Q2: Por que devemos evitar o uso de SELECT \* (selecionar todas as colunas) em aplicações de alta performance?  
R: SELECT \* traz todas as colunas da tabela, muitas das quais podem não ser usadas pelo programa. Isso consome memória desnecessária no servidor de aplicação e aumenta o volume de dados trafegados na rede entre o Banco e o ABAP. A boa prática é selecionar explicitamente apenas os campos (FIELDS) necessários.  
Q3: Qual a diferença entre sy-subrc e sy-dbcnt após um SELECT?  
R: sy-subrc indica o sucesso da operação (0 \= achou, 4 \= não achou). sy-dbcnt indica a quantidade de registros processados (ex: 10 linhas encontradas). É comum verificar sy-subrc para controle de fluxo e sy-dbcnt para relatórios ou validação de volume.  
Q4: Quando devo usar SELECT SINGLE em vez de SELECT ... UP TO 1 ROWS?  
R: Use SELECT SINGLE quando você puder fornecer a chave primária completa para buscar um registro específico de forma inequívoca. Use UP TO 1 ROWS quando você quiser uma amostragem ou o "primeiro registro" de um conjunto maior, geralmente combinado com ORDER BY (ex: buscar o pedido mais recente de um cliente).