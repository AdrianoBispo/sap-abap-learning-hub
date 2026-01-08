# **Módulo 01: Programação ABAP Básica**

## **Aula 02: Aplicando Técnicas e Conceitos Básicos**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Dominar o uso de **Declarações Inline** para variáveis e *Field Symbols*, compreendendo regras de escopo, inferência de tipos complexos e o impacto na legibilidade do código.  
2. Manipular textos de forma avançada utilizando **String Templates**, explorando formatações de data, número, conversão ALPHA, alinhamento e chamadas de métodos embutidas.  
3. Aplicar estruturas de controle de fluxo modernas, substituindo lógicas verbosas por operadores construtores como **COND**, **SWITCH** e introduzindo o uso de **LET** para variáveis auxiliares.  
4. Utilizar o operador **VALUE** para construção rápida de estruturas e tabelas.  
5. Utilizar funções embutidas para operações de string e lógica booleana, alinhando-se aos princípios de *Clean Code* e evitando a criação de variáveis globais desnecessárias.

### **1\. Declarações de Variáveis: O Jeito Moderno (Inline Declarations)**

A introdução das declarações em linha (Inline Declarations) na versão 7.40 foi um verdadeiro divisor de águas para a linguagem ABAP. No modelo clássico, a separação rígida entre a declaração de dados (DATA) no topo do código e a lógica de execução forçava o desenvolvedor a um constante "sobe e desce" na tela para verificar tipos e tamanhos de variáveis.

No ABAP Moderno, a filosofia é clara: **Declare onde você usa.** Isso aproxima a definição da variável do seu contexto de uso, facilitando a leitura e a refatoração.

#### **Inferência de Tipo e Detalhes Técnicos**

O operador de declaração inline DATA(...) não é mágica; ele instrui o compilador a deduzir o tipo de dados com base estritamente no lado direito da atribuição.

* Variáveis Elementares e Literais:  
  Ao atribuir literais, o sistema assume tipos padrão que podem precisar de atenção.  
  DATA(lv\_text) \= 'Texto'.      " Infere tipo c (char) de tamanho fixo baseado no texto  
  DATA(lv\_string) \= \`Texto\`.    " Infere tipo string (dinâmico) devido à crase  
  DATA(lv\_number) \= 100\.        " Infere i (inteiro)

* Estruturas e Objetos (O Grande Ganho de Produtividade):  
  Imagine chamar um método de uma BAPI ou classe standard que retorna uma estrutura complexa com 50 campos ou mais. Antigamente, você teria que abrir a SE11, verificar a estrutura de retorno, declarar uma work area manualmente e só então chamar o método.  
  Com declaração inline, o compilador faz o trabalho pesado:  
  " O sistema cria ls\_result automaticamente com a estrutura exata do retorno  
  DATA(ls\_result) \= lo\_objeto-\>get\_complex\_data( ).

  " Funciona inclusive para tabelas internas  
  SELECT \* FROM flight\_schedule INTO TABLE @DATA(lt\_flights).

* Casting durante a Declaração:  
  Às vezes, a inferência automática não é suficiente (ex: o método retorna um tipo genérico DATA, mas você sabe que é um objeto específico). Você pode combinar inline com casting:  
  DATA(lo\_alv) \= CAST cl\_gui\_alv\_grid( lo\_container-\>get\_content( ) ).

#### **Field Symbols Inline e Performance**

Também podemos declarar ponteiros (Field Symbols) diretamente em loops. Isso não apenas economiza linhas, mas incentiva o uso de referências em vez de cópias de valor, o que é crucial para performance em grandes tabelas.

* **Comparativo de Memória:**  
  " Clássico (WORK AREA): Copia todo o conteúdo da linha para uma nova área de memória.  
  " Lento para estruturas largas.  
  LOOP AT lt\_tabela INTO DATA(ls\_copia).   
  ENDLOOP.

  " Moderno (FIELD-SYMBOL): Cria apenas um ponteiro para a linha existente.  
  " Muito mais rápido e permite modificar a tabela diretamente.  
  LOOP AT lt\_tabela ASSIGNING FIELD-SYMBOL(\<fs\_linha\>).  
    \<fs\_linha\>-status \= 'X'.  
  ENDLOOP.

**Nota Crítica sobre Escopo:** É um erro comum pensar que uma variável declarada inline dentro de um bloco IF ou LOOP deixa de existir quando o bloco fecha. No ABAP, o escopo é o **método** (ou form/function) inteiro.

IF lv\_condition \= abap\_true.  
  DATA(lv\_temp) \= 5\.  
ENDIF.

" A variável lv\_temp AINDA EXISTE aqui e pode ser acessada, o que pode causar bugs  
" se o desenvolvedor não estiver atento. Mantenha a disciplina de nomes\!  
lv\_temp \= 10\. 

### **2\. Manipulação de Strings: String Templates**

O comando CONCATENATE era limitado, verboso e difícil de ler quando envolvia muitas variáveis e espaços. Os **String Templates**, delimitados por barras verticais (| ... |), trouxeram o poder das linguagens modernas para o ABAP.

#### **Interpolação, Cálculos e Chamadas de Método**

A grande vantagem é a capacidade de realizar processamento *dentro* da string. Qualquer expressão ABAP válida pode ser colocada entre chaves { ... }.

DATA(lv\_nome) \= 'Ana'.  
DATA(lv\_sobrenome) \= 'Silva'.

" Concatenação limpa (Espaços são respeitados literalmente)  
DATA(lv\_completo) \= |Prezado cliente: { lv\_nome } { lv\_sobrenome }|.

" Lógica embutida: Chamando métodos funcionais dentro do texto  
DATA(lv\_log) \= |O usuário { lo\_user-\>get\_name( ) } acessou em { cl\_abap\_context\_info=\>get\_system\_time( ) }|.

" Expressões condicionais embutidas  
DATA(lv\_status\_txt) \= |O aluno foi { COND \#( WHEN lv\_nota \> 7 THEN 'Aprovado' ELSE 'Reprovado' ) }|.

#### **Opções de Formatação (Formatting Options)**

Os templates suportam parâmetros de formatação que eliminam a necessidade de chamar Functions Modules de conversão externas (como CONVERSION\_EXIT\_...).

* Datas e Números (Internacionalização):  
  O ABAP ajusta automaticamente o formato baseando-se nas configurações do usuário logado.  
  DATA(lv\_hoje) \= cl\_abap\_context\_info=\>get\_system\_date( ).

  " Formato Técnico (YYYY-MM-DD)  
  out-\>write( |Data ISO: { lv\_hoje DATE \= ISO }| ).   

  " Formato do Usuário (ex: 31.12.2023 no Brasil, 12/31/2023 nos EUA)  
  out-\>write( |Data User: { lv\_hoje DATE \= USER }| ). 

  " Formato de Moeda  
  DATA(lv\_salary) \= 5000\.  
  out-\>write( |Salário: { lv\_salary CURRENCY \= 'BRL' NUMBER \= USER }| ). 

* Conversão ALPHA (Zeros à Esquerda):  
  Essencial para chaves de banco de dados (ex: Cliente, Material, Documento).  
  DATA(lv\_matnr) \= '123'.

  " ALPHA \= IN: Adiciona zeros (Output: 000000000000000123\)  
  DATA(lv\_db\_format) \= |{ lv\_matnr ALPHA \= IN }|. 

  " ALPHA \= OUT: Remove zeros (Output: 123\)  
  DATA(lv\_screen\_format) \= |{ lv\_db\_format ALPHA \= OUT }|. 

* Alinhamento e Preenchimento (Padding):  
  Útil para gerar arquivos de texto posicional (CNAB, layouts bancários).  
  " Alinha à direita, largura 10, preenche com zero: '00000Texto'  
  out-\>write( |{ 'Texto' WIDTH \= 10 ALIGN \= RIGHT PAD \= '0' }| ). 

### **3\. Estruturas de Controle e Operadores Construtores**

Além de modernizar o IF e CASE com operadores simbólicos (=, \<\>, \<=), o ABAP moderno introduziu **Operadores Construtores**. Eles permitem "construir" resultados complexos em uma única linha de comando.

#### **Operador COND (O "IF" Funcional)**

Usado para atribuir valores baseados em condições. Diferente do IF, ele deve retornar um resultado para ser atribuído.

* **Antigo (Verboso):**  
  IF lv\_idade \< 12\.  
    lv\_fase \= 'Criança'.  
  ELSEIF lv\_idade \< 18\.  
    lv\_fase \= 'Adolescente'.  
  ELSE.  
    lv\_fase \= 'Adulto'.  
  ENDIF.

* Moderno (Conciso):  
  Note o uso de \# que significa "infira o tipo da variável à esquerda".  
  DATA(lv\_fase) \= COND string( WHEN lv\_idade \< 12 THEN 'Criança'  
                               WHEN lv\_idade \< 18 THEN 'Adolescente'  
                               ELSE 'Adulto' ).

#### **Operador SWITCH (O "CASE" Funcional)**

Ideal quando a decisão é baseada em valores específicos de uma única variável.

DATA(lv\_cor\_semaforo) \= SWITCH string( lv\_status  
                          WHEN 'S' THEN 'Verde'   " Success  
                          WHEN 'E' THEN 'Vermelho'" Error  
                          WHEN 'W' THEN 'Amarelo' " Warning  
                          ELSE 'Cinza' ).         " Default

#### **Expressão LET (Variáveis Locais Temporárias)**

Uma das adições mais poderosas. O LET permite definir variáveis auxiliares dentro de um construtor (COND, SWITCH, VALUE) que só existem durante aquela operação. Isso evita poluir o código com variáveis temporárias globais.

" Calcula desconto baseado na média de compras, sem criar variável para a média  
DATA(lv\_discount) \= COND i(   
    LET media \= ( lv\_compra1 \+ lv\_compra2 ) / 2   
    IN   
    WHEN media \> 1000 THEN 20  
    WHEN media \> 500  THEN 10  
    ELSE 0 ).

#### **Operador VALUE (Construção de Estruturas)**

Permite inicializar estruturas e tabelas internas de uma vez só.

TYPES: BEGIN OF ty\_user,  
         id   TYPE i,  
         name TYPE string,  
       END OF ty\_user.

" Cria e preenche a estrutura em uma linha  
DATA(ls\_user) \= VALUE ty\_user( id \= 1 name \= 'João' ).

#### **Funções Predicativas**

O ABAP clássico exigia truques para verificar condições booleanas.

* **xsdbool( log\_exp )**: Retorna abap\_true ('X') ou abap\_false (' ') baseado em uma expressão lógica.  
  " Passa 'X' para o método se a idade for maior que 18  
  lo\_class-\>set\_adult\_flag( xsdbool( lv\_age \>= 18 ) ).

* **line\_exists( ... )**: Verifica se uma linha existe numa tabela interna sem precisar fazer um READ TABLE e checar o sy-subrc.  
  IF line\_exists( lt\_users\[ id \= 99 \] ).  
    " Usuário existe...  
  ENDIF.

### **4\. Exemplo Prático: Calculadora Robusta com Histórico**

Este exemplo expandido utiliza SWITCH para a lógica, VALUE para manipular tabelas internas e String Templates avançados.

CLASS zcl\_basic\_concepts DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
      
    " Definindo tipos locais para o exemplo  
    TYPES: BEGIN OF ty\_log,  
             operation TYPE string,  
             val1      TYPE i,  
             val2      TYPE i,  
             result    TYPE decfloat34,  
             timestamp TYPE timestampl,  
           END OF ty\_log,  
           tt\_log TYPE STANDARD TABLE OF ty\_log WITH EMPTY KEY.

  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_basic\_concepts IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " 1\. Setup de Variáveis  
    DATA(lv\_num1) \= 10\.  
    DATA(lv\_num2) \= 5\.  
    DATA(lv\_operation) \= 'DIV'.   
    DATA lt\_history TYPE tt\_log.

    " 2\. Lógica com Operador SWITCH e Tratamento de Exceção Inline  
    " decfloat34 garante alta precisão para cálculos financeiros/científicos  
    DATA(lv\_result) \= SWITCH decfloat34( lv\_operation  
                        WHEN 'SUM' THEN lv\_num1 \+ lv\_num2  
                        WHEN 'SUB' THEN lv\_num1 \- lv\_num2  
                        WHEN 'MULT' THEN lv\_num1 \* lv\_num2  
                        WHEN 'DIV' THEN   
                            " Condicional aninhado para evitar dump de divisão por zero  
                            COND \#( WHEN lv\_num2 \<\> 0   
                                    THEN lv\_num1 / lv\_num2   
                                    ELSE 0 )   
                        ELSE \-1 ).

    " 3\. Validação  
    IF lv\_result \= \-1 AND lv\_operation \<\> 'SUM' AND lv\_operation \<\> 'SUB'.  
       out-\>write( |Erro Crítico: Operação '{ lv\_operation }' desconhecida.| ).  
       RETURN.  
    ENDIF.

    " 4\. Saída Formatada  
    " NUMBER \= USER formata conforme perfil do usuário (ex: 1.000,00)  
    out-\>write( |Resultado da { lv\_operation }: { lv\_result NUMBER \= USER }| ).

    " 5\. Uso do Operador VALUE com BASE para adicionar à tabela  
    " Adiciona uma nova linha à tabela lt\_history preservando o conteúdo existente (BASE)  
    GET TIME STAMP FIELD DATA(lv\_now).  
      
    lt\_history \= VALUE \#( BASE lt\_history   
                        ( operation \= lv\_operation   
                          val1      \= lv\_num1   
                          val2      \= lv\_num2   
                          result    \= lv\_result  
                          timestamp \= lv\_now ) ).

    " 6\. Iteração com DO e LET  
    out-\>write( '--- Tabuada Dinâmica \---' ).  
    DO 3 TIMES.  
        " LET define variáveis auxiliares para tornar o template mais limpo  
        out-\>write( |Cálculo { sy-index }: {   
            COND string( LET fator \= sy-index   
                             calc  \= lv\_num1 \* fator   
                         IN   
                         WHEN calc \> 15 THEN |{ calc } (Alto)|   
                         ELSE |{ calc } (Baixo)| ) }| ).  
    ENDDO.

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Inline Declaration (DATA(...)):** Recurso do ABAP 7.40+ que delega a definição do tipo de dados ao compilador no momento da atribuição. Melhora a fluidez da codificação, mas exige atenção ao escopo da variável, que persiste até o final do método.  
* **String Templates (|...|):** Mecanismo avançado de manipulação de strings que suporta interpolação de variáveis {var}, chamadas de métodos, expressões lógicas e formatação (data, número, alpha) diretamente no literal de texto.  
* **Constructor Operators (COND, SWITCH, VALUE):** Família de operadores funcionais que permitem construir valores, estruturas ou tabelas em uma única instrução, substituindo blocos procedurais extensos de IF/CASE/LOOP.  
* **LET Expression:** Cláusula usada dentro de operadores construtores para definir variáveis locais temporárias. Essencial para evitar cálculos repetitivos e melhorar a legibilidade de expressões complexas.  
* **xsdbool:** Função embutida que converte o resultado de uma expressão lógica (True/False do Kernel) para o tipo ABAP abap\_bool ('X' ou ' '), permitindo o uso de lógica booleana em parâmetros de métodos.  
* **Type Inference (Inferência de Tipo):** Capacidade do compilador de determinar automaticamente o tipo técnico (ex: I, STRING, TYPE REF TO) de uma nova variável baseando-se no valor ou objeto à direita da atribuição.

#### **Tabela Comparativa: Sintaxe Clássica vs. Moderna**

| Conceito | ABAP Clássico (Legacy) | ABAP Moderno (Recomendado) |
| :---- | :---- | :---- |
| **Declaração** | DATA: lv\_val TYPE i. | DATA(lv\_val) \= 10\. |
| **Ponteiro** | FIELD-SYMBOLS \<fs\> TYPE any. | ASSIGNING FIELD-SYMBOL(\<fs\>). |
| **Concatenação** | CONCATENATE a b INTO c. | \`c \= |
| **Condicional** | IF a EQ b. | IF a \= b. |
| **Atribuição Lógica** | Bloco IF/ELSE de várias linhas | DATA(x) \= COND \#( WHEN a \> b ... ). |
| **Estrutura** | ls\_data-campo \= val. (linha a linha) | ls\_data \= VALUE \#( campo \= val ). |
| **Conversão Alpha** | CALL FUNCTION 'CONVERSION\_EXIT\_ALPHA\_INPUT' | \` |
| **Verificar Tabela** | READ TABLE ... TRANSPORTING NO FIELDS | IF line\_exists( ... ). |

### **📝 Quiz de Fixação**

Q1: Qual é a principal vantagem de usar String Templates (|...|) em vez de CONCATENATE, além da legibilidade?  
R: String Templates oferecem opções de formatação embutidas e expressões. É possível converter formatos de data (DATE \= ISO), números (NUMBER \= USER), realizar conversões Alpha (ALPHA \= IN/OUT) e até executar lógica (COND, chamadas de método) diretamente dentro da string, eliminando variáveis auxiliares.  
Q2: O operador COND pode substituir qualquer comando IF?  
R: Não. O COND é um operador construtor, projetado para retornar um valor a ser atribuído a uma variável ou passado como parâmetro. Ele substitui a lógica de IF usada para atribuição de valores. Para controle de fluxo de execução (ex: chamar métodos diferentes, sair de um loop ou encerrar o programa), o comando IF tradicional ainda é necessário.  
Q3: Qual a função da expressão LET dentro de um construtor COND ou VALUE?  
R: A expressão LET permite definir variáveis locais temporárias válidas apenas dentro daquele construtor. Isso é útil para armazenar resultados intermediários de cálculos ou chamadas de métodos, evitando que sejam recalculados múltiplas vezes dentro das condições WHEN, melhorando a performance e a clareza.  
Q4: O que acontece com uma variável declarada inline (DATA(...)) dentro de um loop DO ou LOOP após o término do loop?  
R: A variável permanece acessível. No ABAP, o escopo de uma variável inline é o bloco de processamento atual (método, função ou form). Ela não "morre" ao final do loop. Reutilizar o mesmo nome de variável em loops subsequentes pode levar a erros de tipo ou valor residual se não for gerenciado com cuidado (ex: usando CLEAR).