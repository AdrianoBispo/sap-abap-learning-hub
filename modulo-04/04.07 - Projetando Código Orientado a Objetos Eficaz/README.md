# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 07: Projetando Código Orientado a Objetos Eficaz**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Aplicar o princípio de **Desacoplamento** utilizando **Interfaces Globais**, compreendendo como elas atuam como contratos firmes entre diferentes partes do sistema, permitindo a substituição de componentes sem quebrar a aplicação.  
2. Diferenciar estrategicamente **Composição** ("Tem um") de **Herança** ("É um"), sabendo identificar quando a herança cria um acoplamento rígido indesejado e quando a composição oferece maior flexibilidade.  
3. Implementar o padrão de projeto **Factory Method** para centralizar a lógica de criação de objetos, facilitando a manutenção e permitindo a injeção de dependências dinâmica.  
4. Substituir o tratamento de erro procedural antigo (baseado em sy-subrc) por **Class-Based Exceptions** (CX\_\*), aproveitando a hierarquia de exceções para criar um fluxo de erro robusto e rico em informações.

### **1\. Interfaces: O Contrato do Código**

Muitos desenvolvedores ABAP iniciam na Orientação a Objetos focando excessivamente em classes (ZCL\_...) e herança, negligenciando o poder das Interfaces (ZIF\_...). No entanto, em arquiteturas de software maduras, programar voltado para interfaces, e não para implementações, é um sinal de senioridade.

#### **Herança vs. Interface: A Batalha da Flexibilidade**

* **Herança (Inheritance):** É um relacionamento rígido e estático. Segue a lógica "É um" (Is-a).  
  * *Exemplo:* "Um Cachorro **é um** Animal".  
  * *Limitação:* O ABAP suporta apenas herança simples. Uma classe só pode ter uma "mãe". Se você herda de ZCL\_ANIMAL, não pode herdar de ZCL\_AMIGO ao mesmo tempo. Além disso, mudanças na superclasse afetam todas as subclasses, o que pode introduzir bugs colaterais (Fragile Base Class Problem).  
* **Interface:** É um relacionamento flexível e comportamental. Segue a lógica "Faz algo" (Can-do / Behas-as).  
  * *Exemplo:* "Um Cachorro **faz** Latido" (Implementa ZIF\_LATIDOR). "Um Cachorro **é** Amigável" (Implementa ZIF\_AMIGO).  
  * *Vantagem:* Uma classe pode implementar N interfaces. Isso permite que um objeto desempenhe múltiplos papéis dentro do sistema dependendo do contexto em que é utilizado.

#### **Por que usar Interfaces no RAP e Clean Core?**

Interfaces são a chave para o **Polimorfismo** e o **Desacoplamento**.

1. **Intercambiabilidade:** Se seu código depende de ZIF\_LOG, você pode trocar a implementação de "Log em Banco" para "Log em Arquivo" sem alterar uma única linha do código consumidor.  
2. **Testabilidade:** Para criar Testes Unitários (ABAP Unit), precisamos frequentemente "fingir" o comportamento de dependências complexas (como acesso ao banco de dados). Se seu código usa interfaces, você pode facilmente injetar uma classe "Mock" ou "Stub" durante o teste. Se usar classes concretas, isso se torna muito difícil.

### **2\. O Padrão Factory (Fábrica)**

Um dos maiores inimigos da manutenção é o acoplamento forte criado pelo uso indiscriminado dos comandos CREATE OBJECT ou NEW zcl\_classe( ) espalhados por todo o código.

Quando você escreve DATA(obj) \= NEW zcl\_calculadora\_v1( ), seu código fica "casado" com a versão 1 da calculadora para sempre. Se amanhã você precisar usar a zcl\_calculadora\_v2 baseada em uma configuração do usuário, terá que caçar e alterar todos os lugares onde o NEW foi usado.

A Solução: Factory Method  
A "Fábrica" é uma classe (ou método estático) cuja única responsabilidade é decidir qual objeto criar e como criá-lo. O consumidor não sabe qual classe concreta está recebendo; ele sabe apenas que receberá alguém que obedece a uma Interface.  
CLASS zcl\_travel\_factory DEFINITION PUBLIC.  
  PUBLIC SECTION.  
    " O método retorna a INTERFACE, não a classe concreta.  
    " Isso esconde a complexidade da implementação real.  
    CLASS-METHODS: get\_calculator  
      IMPORTING iv\_country TYPE land1  
      RETURNING VALUE(ro\_calc) TYPE REF TO zif\_travel\_calc.  
ENDCLASS.

CLASS zcl\_travel\_factory IMPLEMENTATION.  
  METHOD get\_calculator.  
    " Lógica de decisão centralizada:  
    " Se precisarmos mudar a classe de cálculo para o Brasil, mudamos APENAS aqui.  
    CASE iv\_country.  
      WHEN 'BR'.  
        ro\_calc \= NEW zcl\_travel\_calc\_br( ).  
      WHEN 'US'.  
        ro\_calc \= NEW zcl\_travel\_calc\_us( ).  
      WHEN OTHERS.  
        ro\_calc \= NEW zcl\_travel\_calc\_global( ).  
    ENDCASE.  
  ENDMETHOD.  
ENDCLASS.

### **3\. Exceções Baseadas em Classe (CX\_\*)**

O tratamento de erros no ABAP evoluiu drasticamente. O modelo antigo usava EXCEPTIONS error \= 1 e verificava a variável de sistema sy-subrc.

* **Problema do Modelo Antigo:** O erro é mudo. O número 1 não diz *por que* o erro ocorreu, não carrega texto, não carrega valores de variáveis no momento do erro e não diz onde (pilha de chamadas) o erro foi originado.

No ABAP Moderno, usamos **Classes de Exceção**, que são objetos ricos em informação herdados da raiz CX\_ROOT.

#### **A Hierarquia de Exceções**

1. **CX\_STATIC\_CHECK (Checada):**  
   * **Comportamento:** O compilador *obriga* o desenvolvedor a tratar (TRY...CATCH) ou propagar (RAISING) a exceção.  
   * **Uso:** Erros de negócio previsíveis onde o programa consumidor deve ter uma chance de se recuperar (ex: "Saldo Insuficiente", "Cliente Bloqueado").  
2. **CX\_NO\_CHECK (Não Checada):**  
   * **Comportamento:** O tratamento é opcional. Se não tratado, gera um Dump automático.  
   * **Uso:** Erros técnicos graves ou de programação onde a recuperação é improvável (ex: "Divisão por Zero", "Ponteiro Nulo", "Parâmetro Obrigatório Vazio").  
3. **CX\_DYNAMIC\_CHECK:**  
   * Um híbrido raramente usado em código de aplicação, comum em frameworks genéricos.

#### **Disparando uma Exceção Rica**

Ao disparar uma exceção, podemos passar parâmetros para explicar o erro.

IF iv\_amount \< 0\.  
  " RAISE EXCEPTION cria o objeto de erro e interrompe o fluxo imediatamente  
  RAISE EXCEPTION TYPE zcx\_travel\_error  
    EXPORTING  
      textid    \= zcx\_travel\_error=\>negative\_amount " Mensagem específica T100  
      mv\_amount \= iv\_amount  " Passamos o valor inválido para log/análise  
      previous  \= lx\_prev\_exception. " Encadeamento de erros (opcional)  
ENDIF.

### **4\. Exemplo Prático: Refatoração para OO Limpo**

Vamos transformar um código procedural, cheio de IFs e dependências rígidas, em um design orientado a objetos robusto utilizando o padrão **Strategy**.

**Cenário:** Precisamos calcular o desconto de uma viagem.

* Clientes **VIP** recebem 20% de desconto.  
* Clientes **Standard** não recebem desconto.  
* No futuro, novos tipos de clientes (ex: "Partner") podem surgir.

#### **Passo A: A Interface (O Contrato)**

Primeiro, definimos *o que* precisa ser feito, ignorando *como*.

INTERFACE zif\_discount\_strategy PUBLIC.  
  " Qualquer classe que queira ser uma estratégia de desconto  
  " deve saber executar este método.  
  METHODS calculate  
    IMPORTING iv\_price TYPE p  
    RETURNING VALUE(rv\_discount) TYPE p.  
ENDINTERFACE.

#### **Passo B: As Implementações (Polimorfismo)**

Criamos classes pequenas e focadas para cada regra de negócio.

" Estratégia para VIP  
CLASS zcl\_discount\_vip DEFINITION PUBLIC.  
  PUBLIC SECTION.  
    INTERFACES zif\_discount\_strategy.  
ENDCLASS.

CLASS zcl\_discount\_vip IMPLEMENTATION.  
  METHOD zif\_discount\_strategy\~calculate.  
    " Regra isolada: VIP ganha 20%  
    rv\_discount \= iv\_price \* '0.2'.   
  ENDMETHOD.  
ENDCLASS.

" Estratégia para Standard  
CLASS zcl\_discount\_std DEFINITION PUBLIC.  
  PUBLIC SECTION.  
    INTERFACES zif\_discount\_strategy.  
ENDCLASS.

CLASS zcl\_discount\_std IMPLEMENTATION.  
  METHOD zif\_discount\_strategy\~calculate.  
    " Regra isolada: Standard ganha 0  
    rv\_discount \= 0\.  
  ENDMETHOD.  
ENDCLASS.

#### **Passo C: O Consumo (Injeção de Dependência e Factory Simples)**

O programa principal (Consumidor) não contém a lógica de cálculo. Ele apenas orquestra. Ele não sabe se está calculando para VIP ou Standard; ele confia na Interface.

CLASS zcl\_main\_process IMPLEMENTATION.  
  METHOD process\_booking.  
    " Definimos a variável com o tipo da INTERFACE, não da classe concreta  
    DATA: lo\_strategy TYPE REF TO zif\_discount\_strategy.

    " 1\. Lógica de Criação (Poderia estar numa classe Factory separada)  
    IF iv\_customer\_type \= 'VIP'.  
      lo\_strategy \= NEW zcl\_discount\_vip( ).  
    ELSE.  
      lo\_strategy \= NEW zcl\_discount\_std( ).  
    ENDIF.

    " 2\. Execução Polimórfica  
    " O método 'calculate' se comporta de maneira diferente dependendo  
    " de qual objeto está dentro de 'lo\_strategy'.  
    DATA(lv\_discount) \= lo\_strategy-\>calculate( 1000 ).  
      
    out-\>write( |Desconto aplicado: { lv\_discount }| ).  
  ENDMETHOD.  
ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Interface (ZIF):** Um artefato de desenvolvimento que contém apenas assinaturas de métodos (definições), sem implementação. Serve como um contrato que obriga as classes implementadoras a fornecerem a lógica real. Permite o desacoplamento entre quem chama (consumidor) e quem executa (provedor).  
* **Polymorphism (Polimorfismo):** Capacidade de objetos de classes diferentes (ex: zcl\_discount\_vip e zcl\_discount\_std) responderem à mesma mensagem (chamada de método definida na Interface) de maneiras diferentes. É o pilar da flexibilidade em OO.  
* **Factory Pattern:** Padrão de criação onde a instância exata de um objeto é decidida em tempo de execução por uma classe "fábrica", em vez de ser fixada no código do consumidor. Facilita a manutenção e a introdução de novas variantes sem alterar o código existente.  
* **Exception Class (CX):** Classes usadas para gerenciar erros no ABAP Moderno. Elas transportam o contexto do erro (TextID, atributos, call stack) do ponto de falha até o manipulador (CATCH).  
* **Separation of Concerns (Separação de Preocupações):** Princípio de design onde cada classe ou módulo deve ter uma responsabilidade única e clara. No exemplo, a classe VIP só sabe calcular desconto VIP; ela não sabe sobre banco de dados ou impressão.

#### **Tabela de Decisão: Herança vs Interface**

| Cenário | Usar Herança (INHERITING FROM) | Usar Interface (INTERFACES) |
| :---- | :---- | :---- |
| **Relacionamento Conceitual** | "É um" (Carro é um Veículo) | "Faz algo" / "Possui comportamento" (Carro é Dirigível) |
| **Objetivo Principal** | Reutilizar código existente da classe mãe para evitar duplicação. | Definir um contrato comum para classes distintas conversarem. |
| **Acoplamento** | **Alto/Forte**. Subclasses quebram se a mãe mudar. | **Baixo/Fraco**. A implementação pode mudar livremente desde que respeite o contrato. |
| **Multiplicidade** | **Não**. ABAP só permite 1 superclasse. | **Sim**. Uma classe pode implementar N interfaces. |

### **📝 Quiz de Fixação**

Q1: Qual é a principal vantagem de usar RAISE EXCEPTION TYPE zcx... em vez de MESSAGE 'Erro' TYPE 'E' dentro de uma classe de negócio?  
R: A exceção baseada em classe é um objeto que pode ser capturado (TRY...CATCH) e tratado programaticamente nas camadas superiores da aplicação. Uma mensagem do tipo 'E' (Error) geralmente interrompe o processamento abruptamente ou é difícil de capturar de forma estruturada, comportando-se mais como uma interface de usuário do que como um controle de fluxo. Além disso, a exceção CX carrega atributos detalhados sobre o erro.  
Q2: Por que dizemos que "Programar para Interfaces" facilita a criação de Testes Unitários?  
R: Porque permite a Injeção de Dependência. Se o seu código depende de uma Interface ZIF\_CLIENTE\_DAO em vez da classe concreta ZCL\_CLIENTE\_DAO, você pode, durante o teste, injetar uma classe falsa (Mock) que implementa a mesma interface, mas retorna dados fixos de memória, isolando o teste do banco de dados real.  
Q3: Em que situação devo usar uma exceção que herda de CX\_NO\_CHECK?  
R: Exceções CX\_NO\_CHECK devem ser usadas para erros técnicos graves ou situações inesperadas onde não se espera que o programa consumidor consiga se recuperar ou tratar o erro localmente (ex: falta de memória, argumentos nulos em métodos internos). Elas não obrigam o uso de TRY...CATCH e geralmente resultam em um Dump informativo se não tratadas no topo da pilha.