# **Módulo 01: Programação ABAP Básica**

## **Aula 03: Trabalhando com Classes Locais e Orientação a Objetos**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Distinguir claramente entre **Classes Globais** e **Classes Locais**, compreendendo os casos de uso ideais para cada uma.  
2. Dominar a anatomia de uma classe: separar a **Definição** (Interface/Contrato) da **Implementação** (Lógica/Código).  
3. Aplicar estrategicamente as seções de visibilidade (PUBLIC, PROTECTED, PRIVATE) para garantir o encapsulamento e segurança do código.  
4. Utilizar a sintaxe moderna de instanciação com o operador NEW, incluindo a passagem de parâmetros para o construtor.  
5. Entender a diferença entre membros de **Instância** e membros **Estáticos**.

### **1\. Classes Globais vs. Classes Locais: Onde codificar?**

No ecossistema ABAP, a Orientação a Objetos (OO) é a espinha dorsal do desenvolvimento moderno (RAP). Embora a sintaxe seja idêntica, o escopo de uso difere:

* **Classes Globais (Global Classes \- Prefixo ZCL\_):**  
  * **Definição:** Criadas através do ABAP Development Tools (ADT/Eclipse) ou SE24. São objetos de repositório independentes.  
  * **Visibilidade:** Visíveis por todo o sistema SAP. Qualquer programa, função ou outra classe pode instanciá-las.  
  * **Uso:** Lógica de negócio reutilizável, APIs públicas, Entidades de Negócio.  
* **Classes Locais (Local Classes \- Prefixo LCL\_):**  
  * **Definição:** Definidas *dentro* de um artefato maior (como um Programa Executável, uma Function Group ou, o mais comum, dentro da aba "Local Types" de uma Classe Global).  
  * **Visibilidade:** Restrita. Elas só existem dentro do artefato que as contém. Uma classe local definida dentro da classe ZCL\_A não pode ser vista pela classe ZCL\_B.  
  * **Uso:**  
    * **Classes Auxiliares (Helpers):** Para quebrar uma lógica complexa interna sem poluir o repositório global com classes que só servem para uma tarefa específica.  
    * **Testes Unitários (ABAP Unit):** Este é o uso mais crítico. Todos os testes unitários são escritos como classes locais (FOR TESTING) que simulam o comportamento da classe principal.

### **2\. A Anatomia de uma Classe: Contrato vs. Ação**

Uma classe ABAP não é um bloco monolítico. Ela é dividida em duas partes obrigatórias que funcionam como uma promessa e seu cumprimento.

#### **A. DEFINITION (A Promessa / Contrato)**

Aqui descrevemos a "interface" da classe. Definimos os tipos de dados, as constantes e as assinaturas dos métodos (parâmetros de entrada e saída). Nenhuma linha de lógica executável (como IF, LOOP) entra aqui.

CLASS lcl\_exemplo DEFINITION.  
  PUBLIC SECTION.  
    DATA: mv\_nome TYPE string.      " Atributo de Instância  
    CLASS-DATA: gv\_contador TYPE i. " Atributo Estático (Compartilhado)  
      
    METHODS: constructor IMPORTING iv\_nome TYPE string. " Método Especial  
    METHODS: executar.  
ENDCLASS.

#### **B. IMPLEMENTATION (A Ação / Lógica)**

Aqui escrevemos o código ABAP real. Cada método declarado na DEFINITION deve ter sua correspondente implementação aqui.

CLASS lcl\_exemplo IMPLEMENTATION.  
  METHOD constructor.  
    mv\_nome \= iv\_nome.  
    gv\_contador \= gv\_contador \+ 1\. " Incrementa contador global da classe  
  ENDMETHOD.

  METHOD executar.  
    " Lógica de negócio...  
  ENDMETHOD.  
ENDCLASS.

#### **Seções de Visibilidade (Encapsulamento)**

O encapsulamento é vital para manutenção. Se tudo for público, qualquer desenvolvedor pode alterar variáveis internas da sua classe, causando bugs inesperados.

1. **PUBLIC SECTION:** A vitrine da loja. Métodos e atributos que o mundo externo precisa acessar. Define a API estável da classe.  
2. **PROTECTED SECTION:** A área da família. Acessível pela própria classe e por suas classes filhas (herança). Usado para permitir que subclasses reutilizem lógica interna sem expô-la ao mundo.  
3. **PRIVATE SECTION:** O cofre. Acessível *apenas* pela própria classe. É onde escondemos a complexidade. Se você mudar a lógica de um método privado, tem a garantia de que nenhum código externo quebrará, pois ninguém de fora consegue chamá-lo.

### **3\. Instanciação Moderna: O Operador NEW**

A criação de objetos evoluiu para tornar o código mais fluído e legível.

#### **Sintaxe Antiga vs. Moderna**

* **Antigo (CREATE OBJECT):** Exigia a declaração prévia da variável com o tipo exato, ocupando várias linhas.  
  DATA: lo\_cliente TYPE REF TO lcl\_cliente.  
  CREATE OBJECT lo\_cliente  
    EXPORTING  
      iv\_id \= '100'.

* **Moderno (NEW):** Permite instanciação inline. O tipo é inferido (\#) ou explícito.  
  " Inferência de tipo (se o lado esquerdo já estiver tipado ou for claro)  
  DATA(lo\_cliente) \= NEW lcl\_cliente( iv\_id \= '100' ).

  " Uso direto em chamadas de método (sem variável auxiliar\!)  
  lo\_fatura-\>processar( io\_cliente \= NEW lcl\_cliente( '100' ) ).

#### **O Método CONSTRUCTOR**

Ao usar NEW, o método especial constructor da classe é chamado automaticamente.

* Ele é usado para **inicializar** o objeto (ex: carregar dados obrigatórios).  
* Se o construtor tiver parâmetros IMPORTING, eles devem ser passados dentro dos parênteses do NEW ... ( ).

### **4\. Exemplo Prático Expandido: Calculadora de IMC com Estado**

Neste exemplo avançado, criamos uma classe local que possui um **Construtor** para configurar a unidade de medida (Métrica ou Imperial) e mantemos o estado interno.

" \-----------------------------------------------------------------------  
" 1\. DEFINIÇÃO DA CLASSE LOCAL  
" \-----------------------------------------------------------------------  
CLASS lcl\_bmi\_service DEFINITION.  
  PUBLIC SECTION.  
    " Enumeração simples para tipos de unidade  
    CONSTANTS:  
      BEGIN OF co\_unit,  
        metric   TYPE char1 VALUE 'M', " Metros/Kg  
        imperial TYPE char1 VALUE 'I', " Polegadas/Libras  
      END OF co\_unit.

    TYPES: ty\_bmi TYPE p LENGTH 8 DECIMALS 2\.

    " O Construtor define o estado inicial do objeto  
    METHODS: constructor  
      IMPORTING iv\_unit\_type TYPE char1 DEFAULT co\_unit-metric.

    METHODS: calculate\_bmi  
      IMPORTING  
        iv\_weight     TYPE p  
        iv\_height     TYPE p  
      RETURNING  
        VALUE(rv\_bmi) TYPE ty\_bmi.

  PRIVATE SECTION.  
    " Atributo privado para guardar a configuração da unidade  
    DATA: mv\_unit\_type TYPE char1.

    " Método auxiliar privado (Encapsulamento)  
    METHODS: convert\_to\_metric  
      IMPORTING iv\_val        TYPE p  
                iv\_type       TYPE char1  
      RETURNING VALUE(rv\_val) TYPE p.  
ENDCLASS.

" \-----------------------------------------------------------------------  
" 2\. IMPLEMENTAÇÃO DA CLASSE LOCAL  
" \-----------------------------------------------------------------------  
CLASS lcl\_bmi\_service IMPLEMENTATION.

  METHOD constructor.  
    " Guarda a preferência de unidade na instância  
    mv\_unit\_type \= iv\_unit\_type.  
  ENDMETHOD.

  METHOD calculate\_bmi.  
    DATA: lv\_weight\_kg TYPE p DECIMALS 2,  
          lv\_height\_m  TYPE p DECIMALS 2\.

    " Normaliza os dados baseando-se na configuração do objeto  
    IF mv\_unit\_type \= co\_unit-metric.  
      lv\_weight\_kg \= iv\_weight.  
      lv\_height\_m  \= iv\_height.  
    ELSE.  
      " Conversão simplificada para Imperial  
      lv\_weight\_kg \= iv\_weight \* '0.453'. " Libras para Kg  
      lv\_height\_m  \= iv\_height \* '0.025'. " Polegadas para Metros  
    ENDIF.

    " Proteção contra divisão por zero  
    IF lv\_height\_m \<= 0\.  
      rv\_bmi \= 0\.  
      RETURN.  
    ENDIF.

    " Cálculo final (Sempre em métrico internamente)  
    rv\_bmi \= lv\_weight\_kg / ( lv\_height\_m \* lv\_height\_m ).  
  ENDMETHOD.

  METHOD convert\_to\_metric.  
    " Implementação futura se necessário...  
    rv\_val \= iv\_val.  
  ENDMETHOD.

ENDCLASS.

" \-----------------------------------------------------------------------  
" 3\. CLASSE GLOBAL (Consumidor)  
" \-----------------------------------------------------------------------  
CLASS zcl\_health\_app DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
ENDCLASS.

CLASS zcl\_health\_app IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.  
      
    " A. Instanciando configurado para sistema MÉTRICO (Padrão)  
    DATA(lo\_metric\_calc) \= NEW lcl\_bmi\_service( ).   
      
    DATA(lv\_bmi\_br) \= lo\_metric\_calc-\>calculate\_bmi(   
        iv\_weight \= 80     " 80kg  
        iv\_height \= '1.80' " 1.80m  
    ).  
    out-\>write( |IMC (Brasil): { lv\_bmi\_br }| ).

    " B. Instanciando configurado para sistema IMPERIAL  
    " Passamos o parâmetro para o CONSTRUTOR aqui  
    DATA(lo\_usa\_calc) \= NEW lcl\_bmi\_service(   
        iv\_unit\_type \= lcl\_bmi\_service=\>co\_unit-imperial   
    ).

    DATA(lv\_bmi\_us) \= lo\_usa\_calc-\>calculate\_bmi(   
        iv\_weight \= 176  " \~80kg em libras  
        iv\_height \= 70   " \~1.78m em polegadas  
    ).  
    out-\>write( |IMC (USA): { lv\_bmi\_us }| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Instance (Instância):** A concretização de uma classe na memória. Enquanto a classe é o projeto (blueprint), a instância é o objeto real com seus próprios dados. Múltiplas instâncias da mesma classe podem coexistir com dados diferentes.  
* **Constructor (Construtor):** Método especial (constructor) executado automaticamente no momento da criação do objeto (NEW). Usado para configurar o estado inicial e validar dependências obrigatórias.  
* **Static vs. Instance Members:**  
  * **Instance:** Pertence ao objeto individual (ex: Nome do Cliente). Cada objeto tem o seu.  
  * **Static (CLASS-DATA, CLASS-METHODS):** Pertence à classe inteira. Compartilhado por todas as instâncias (ex: Contador de quantos objetos foram criados).  
* **Encapsulation (Encapsulamento):** Pilar da OO que visa ocultar os detalhes de implementação (Private) e expor apenas uma interface segura (Public), protegendo a integridade dos dados internos.  
* **Method Signature:** A definição completa da interface de um método, incluindo seu nome e todos os parâmetros de entrada, saída e exceções.

#### **Tabela Comparativa: Visibilidade**

| Seção | Acesso Interno | Acesso por Subclasses | Acesso Externo (Público) | Objetivo Principal |
| :---- | :---- | :---- | :---- | :---- |
| **PUBLIC** | ✅ | ✅ | ✅ | Definir a API de uso da classe. |
| **PROTECTED** | ✅ | ✅ | ❌ | Permitir extensão via herança. |
| **PRIVATE** | ✅ | ❌ | ❌ | Ocultar lógica interna (Segurança). |

### **📝 Quiz de Fixação**

Q1: Qual a diferença fundamental entre a DEFINITION e a IMPLEMENTATION de uma classe?  
R: A DEFINITION descreve o contrato da classe (quais métodos e atributos ela possui e sua visibilidade), servindo como um manual de uso. A IMPLEMENTATION contém o código ABAP real (a lógica) que dita como esses métodos funcionam internamente.  
Q2: O que acontece se eu tentar acessar um atributo definido na PRIVATE SECTION a partir de um programa externo?  
R: Ocorrerá um erro de sintaxe. O compilador ABAP impede o acesso direto a membros privados de fora da própria classe, garantindo o encapsulamento.  
Q3: Para que serve o método constructor e quando ele é chamado?  
R: Ele serve para inicializar o objeto, definindo valores padrão ou recebendo configurações iniciais. Ele é chamado automaticamente pelo sistema no momento em que o comando NEW (ou CREATE OBJECT) é executado.  
Q4: Se eu alterar um atributo estático (CLASS-DATA) em uma instância da classe, o que acontece com as outras instâncias?  
R: O valor muda para todas as instâncias. Atributos estáticos são compartilhados globalmente por todos os objetos daquela classe, pois residem na memória da classe, não na memória do objeto individual.