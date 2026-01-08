# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 04: Usando Code Pushdown no ABAP SQL**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Aplicar **Expressões Aritméticas e de String** diretamente na lista de seleção do SELECT, eliminando a necessidade de pós-processamento de dados no servidor de aplicação.  
2. Utilizar lógica condicional complexa com **CASE** dentro de consultas SQL para transformar códigos técnicos em informações de negócio legíveis na fonte.  
3. Realizar agregações estatísticas (**SUM**, **AVG**, **COUNT**, **MIN**, **MAX**) e agrupamentos (**GROUP BY**) para criar relatórios analíticos de alta performance.  
4. Combinar resultados de múltiplas seleções heterogêneas usando **UNION** e **UNION ALL**, compreendendo as implicações de performance e requisitos de compatibilidade de tipos.  
5. Empregar funções de tratamento de nulos como **COALESCE** para garantir a robustez de cálculos matemáticos no banco de dados.

### **1\. ABAP SQL vs. CDS Views: Quando usar qual?**

Já aprendemos a colocar lógica no banco usando CDS Views. Mas e se a lógica for específica demais para uma única rotina? A escolha entre criar uma View no dicionário ou escrever uma query complexa no código depende da **reutilização**.

* **CDS View:** A ferramenta ideal para definir modelos de dados reutilizáveis. Se o cálculo de "Total com Impostos" for usado em três relatórios e um aplicativo Fiori, ele deve estar numa CDS View. É a "Single Source of Truth".  
* **ABAP SQL Moderno:** A ferramenta para lógica específica de um método ou classe. Se você precisa de uma query ad-hoc que combina dados de forma única para um processamento batch específico, não polua o dicionário com uma View que só tem um consumidor. Escreva o SQL complexo diretamente no ABAP.

O ABAP SQL (a partir do 7.50) herdou quase todos os "superpoderes" do CDS, permitindo que a sintaxe seja praticamente idêntica. Isso facilita a refatoração: você pode prototipar no ABAP SQL e depois mover para uma CDS View se a lógica provar ser reutilizável.

### **2\. Cálculos e Expressões na Query**

O padrão antigo de "Selecionar tudo (SELECT \*), jogar na tabela interna e fazer LOOP para calcular" é o maior inimigo da performance em bancos de dados em memória como o SAP HANA. O custo de trazer milhões de células de dados pela rede apenas para somar duas colunas e descartar o resto é proibitivo.

#### **Aritmética no SELECT**

Podemos calcular preços, impostos, margens e datas direto na query. O banco de dados é extremamente eficiente em matemática vetorial.

**Atenção aos Nulos:** Em operações SQL, 5 \+ NULL \= NULL. Para evitar que um campo vazio anule seu cálculo, usamos a função COALESCE( campo, 0 ), que retorna o primeiro valor não nulo (neste caso, zero se o campo for nulo).

SELECT FROM zrap\_travel  
  FIELDS travel\_id,  
         total\_price,  
         booking\_fee,  
           
         " Cálculo direto no banco: Soma simples  
         " Se booking\_fee for nulo, o resultado seria nulo sem tratamento  
         ( total\_price \+ booking\_fee ) AS grand\_total,  
           
         " Cálculo com Literais e Casting  
         " Multiplicamos por 0.9 (literal) para dar 10% de desconto  
         " CAST é usado para garantir que o resultado caiba no tipo de destino  
         CAST( total\_price \* '0.9' AS CURR( 15, 2 ) ) AS discounted\_price,  
           
         " Cálculo de Margem (Divisão)  
         " A função DIV faz divisão inteira, / faz divisão com decimais  
         DIVISION( total\_price, 100, 2 ) AS price\_index

  WHERE currency\_code \= 'EUR'  
  INTO TABLE @DATA(lt\_results).

#### **Strings no SELECT**

Manipulação de texto também pode ser feita no banco, economizando loops ABAP. Além da concatenação, temos funções para caixa alta/baixa, substituição e comprimento.

SELECT FROM /dmo/customer  
  FIELDS customer\_id,  
           
         " Concatenação Simples com operador &&  
         first\_name && last\_name AS raw\_name,  
           
         " Função dedicada com separador (Mais limpo que && ' ' &&)  
         concat\_with\_space( first\_name, last\_name, 1 ) AS full\_name,  
           
         " Normalização para busca (Tudo em Maiúsculo)  
         upper( last\_name ) AS upper\_name,  
           
         " Extração de parte do texto (Primeiras 3 letras)  
         substring( last\_name, 1, 3 ) AS short\_name

  INTO TABLE @DATA(lt\_names).

### **3\. Lógica Condicional (CASE)**

O famoso IF/ELSE dentro do loop pode ser eliminado usando CASE no SQL. Isso permite transformar códigos técnicos (ex: 'X', 'A') em textos descritivos ou categorizações diretamente na extração.

Existem duas formas de CASE:

1. **Simple CASE:** Compara um campo contra valores.  
2. **Complex CASE:** Permite condições lógicas variadas (WHEN a \> b).

SELECT FROM zrap\_travel  
  FIELDS travel\_id,  
         overall\_status,  
         total\_price,  
           
         " Simple CASE: Tradução de Status  
         CASE overall\_status  
           WHEN 'A' THEN 'Aceito'  
           WHEN 'X' THEN 'Rejeitado'  
           WHEN 'O' THEN 'Aberto'  
           ELSE 'Pendente' " Valor padrão se nenhum coincidir  
         END AS status\_text,  
           
         " Complex CASE: Categorização baseada em valores  
         CASE  
           WHEN total\_price \< 1000 THEN 'Econômica'  
           WHEN total\_price BETWEEN 1000 AND 5000 THEN 'Executiva'  
           WHEN total\_price \> 5000 THEN 'Primeira Classe'  
           ELSE 'Não Classificado'  
         END AS price\_category

  INTO TABLE @DATA(lt\_status).

### **4\. Agregações e Agrupamento (GROUP BY)**

Se você precisa de um relatório de totais, **nunca** traga os dados detalhados para o ABAP para somar (usando COLLECT ou Loop). O banco de dados possui índices e otimizações específicas para agregação que são ordens de magnitude mais rápidas que o servidor de aplicação.

* **Funções de Agregação:** SUM(), AVG() (Média), MIN(), MAX(), COUNT().  
* **Regra Fundamental:** Se usar qualquer função de agregação, **qualquer campo** na lista de seleção que *não* for agregado deve obrigatoriamente estar na cláusula GROUP BY. O banco precisa saber "somar o quê por quem".

**HAVING vs. WHERE:**

* WHERE: Filtra os dados **antes** de agrupar (ex: "Considere apenas viagens em Dólar").  
* HAVING: Filtra os dados **depois** de agrupar (ex: "Mostre apenas clientes que gastaram mais de 1 milhão no total").

SELECT FROM zrap\_travel  
  FIELDS customer\_id,  
         currency\_code,  
           
         " Contar quantas viagens existem neste grupo  
         COUNT( \* ) AS total\_travels,  
           
         " Somar o valor total das viagens  
         SUM( total\_price ) AS total\_spent,  
           
         " Encontrar o valor da viagem mais cara e mais barata  
         MAX( total\_price ) AS max\_spent,  
         MIN( total\_price ) AS min\_spent

  " Filtra ANTES de somar (apenas viagens de 2023 em diante)  
  WHERE begin\_date \>= '20230101'  
    
  " Agrupa por Cliente e Moeda (obrigatório pois estão no FIELDS)  
  GROUP BY customer\_id, currency\_code  
    
  " Filtra DEPOIS de somar (apenas "Grandes Clientes")  
  HAVING SUM( total\_price ) \> 50000   
    
  INTO TABLE @DATA(lt\_analytics).

### **5\. Combinando Resultados (UNION)**

Às vezes precisamos juntar dados de duas tabelas diferentes que têm estrutura similar, mas que logicamente estão separadas (ex: Tabela de Vendas Atuais e Tabela de Histórico/Arquivo Morto, ou Clientes Nacionais e Internacionais). O ABAP SQL permite fazer isso em uma única ida ao banco.

* **Requisito:** O número de colunas e os tipos de dados de cada coluna correspondente devem ser compatíveis entre os SELECTs.  
* **UNION ALL:** Junta os resultados das duas queries "como estão". É muito rápido.  
* **UNION:** Junta os resultados e executa um passo extra de **ordenação e remoção de duplicatas**. É mais lento e deve ser usado apenas se você realmente precisar garantir unicidade entre os conjuntos.

" Seleciona Voos Ativos  
SELECT FROM /dmo/connection  
  FIELDS carrier\_id, connection\_id, distance  
  WHERE distance \> 2000

UNION ALL " Junta com...

" Seleciona Voos de uma tabela de histórico (exemplo hipotético)  
SELECT FROM /dmo/conn\_hist  
  FIELDS carrier\_id, connection\_id, distance  
  WHERE distance \> 2000

INTO TABLE @DATA(lt\_all\_long\_flights).

### **6\. Exemplo Prático: Relatório Analítico via Código**

Vamos criar uma classe que gera um relatório de gastos por agência, classificando-as como "VIP" ou "Standard", utilizando todo o poder do Code Pushdown: Agregação, CASE, Aritmética e Filtro Pós-Agregação.

CLASS zcl\_sql\_pushdown DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_sql\_pushdown IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " Query Analítica Complexa  
    " Objetivo: Analisar performance de agências que operam em grandes volumes  
    SELECT FROM zrap\_travel  
      FIELDS agency\_id,  
             currency\_code,  
               
             " 1\. Contagem e Soma (Agregação)  
             COUNT( \* ) AS number\_of\_travels,  
             SUM( total\_price ) AS total\_amount,

             " 2\. Média de preço por viagem (Ticket Médio)  
             " Casting para decimal garante precisão na média  
             AVG( total\_price AS DEC( 15,2 ) ) AS average\_ticket,

             " 3\. Classificação baseada na soma (Code Pushdown Lógico)  
             " O CASE avalia o resultado da agregação SUM()  
             CASE   
               WHEN SUM( total\_price ) \> 100000 THEN 'Platinum Partner'  
               WHEN SUM( total\_price ) \> 10000  THEN 'Gold Partner'  
               ELSE 'Standard Partner'  
             END AS partner\_category

      " Agrupamento Obrigatório para campos não agregados (Agency, Currency)  
      GROUP BY agency\_id, currency\_code  
        
      " Filtro pós-agregação (HAVING):   
      " Removemos agências pequenas para focar a análise  
      HAVING SUM( total\_price ) \> 1000  
        
      " Ordenar do maior faturamento para o menor  
      ORDER BY total\_amount DESCENDING  
      INTO TABLE @DATA(lt\_report).

    " Exibição no Console  
    out-\>write( name \= 'Relatório de Agências' data \= lt\_report ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Aggregation Function (Função de Agregação):** Funções SQL que operam em um conjunto de linhas para retornar um único valor resumido. Exemplos: SUM (Soma), AVG (Média), MAX (Máximo), MIN (Mínimo).  
* **GROUP BY:** Cláusula SQL obrigatória quando se mistura colunas normais e funções de agregação. Ela define os "baldes" onde os dados serão agrupados (ex: agrupar vendas "por Cliente").  
* **HAVING:** Cláusula usada para filtrar resultados *após* a agregação ter sido feita (diferente do WHERE, que filtra *antes*). É usada para condições sobre os valores sumarizados (ex: HAVING SUM(val) \> 100).  
* **UNION / UNION ALL:** Operadores que combinam o conjunto de resultados de duas ou mais instruções SELECT. UNION remove linhas duplicadas (custoso), enquanto UNION ALL mantém todas (rápido).  
* **Coalesce:** Função SQL (coalesce( val1, val2, ... )) que retorna o primeiro valor não nulo de uma lista de argumentos. Indispensável para evitar que valores NULL propaguem e anulem cálculos aritméticos inteiros.  
* **Literals (Literais):** Valores fixos escritos diretamente na query (ex: 'Ativo', 100, 0.1). Podem ser usados em expressões aritméticas, comparações e projeções.

#### **Loop ABAP vs Code Pushdown SQL**

| Cenário | ABAP Clássico (Evitar) | ABAP SQL (Recomendado) | Vantagem SQL |
| :---- | :---- | :---- | :---- |
| **Soma de Totais** | LOOP, acumular em variável. | SELECT SUM(...) | Menor tráfego de rede, uso de índices de coluna. |
| **Status Texto** | LOOP, IF/ELSE, modificar tabela. | SELECT CASE ... | Lógica centralizada, retorno já formatado. |
| **Juntar Tabelas** | Dois SELECTs, LOOP e APPEND. | SELECT ... UNION ... | Uma única ida ao banco (Roundtrip). |
| **Filtro de Soma** | LOOP, calcular, DELETE se menor que X. | SELECT ... HAVING SUM \> X | O banco só retorna o que interessa. |

### **📝 Quiz de Fixação**

Q1: Qual a diferença técnica e de performance entre WHERE e HAVING em uma consulta SQL com agregações?  
R: O WHERE filtra as linhas brutas antes que elas sejam agrupadas e calculadas, reduzindo o volume de dados a ser processado pelo agrupador. O HAVING filtra os resultados já agregados (os grupos) após o cálculo. Para performance, deve-se filtrar o máximo possível no WHERE.  
Q2: Por que UNION ALL é geralmente mais performático que UNION?  
R: Porque o UNION padrão executa um passo adicional e custoso de processamento (sort/distinct) para identificar e remover linhas duplicadas entre os conjuntos de resultados. O UNION ALL simplesmente anexa os resultados sequencialmente, sem verificação extra.  
Q3: Se eu usar a função SUM( price ) na minha lista de campos, o que sou obrigado a fazer com os outros campos (ex: customer\_id) que não estão sendo somados?  
R: Sou obrigado a incluí-los na cláusula GROUP BY. Caso contrário, ocorrerá um erro de sintaxe SQL, pois o banco de dados não sabe como condensar múltiplas linhas de clientes diferentes em uma só linha de soma sem um critério explícito de agrupamento.  
Q4: Para que serve a função COALESCE e em que cenário ela é indispensável?  
R: A função COALESCE retorna o primeiro valor não nulo de uma lista. Ela é indispensável em cálculos aritméticos (somas, multiplicações) onde um dos campos pode ser NULL, pois em SQL qualquer operação com NULL resulta em NULL. O COALESCE permite substituir o nulo por zero ou um valor padrão para que o cálculo prossiga.