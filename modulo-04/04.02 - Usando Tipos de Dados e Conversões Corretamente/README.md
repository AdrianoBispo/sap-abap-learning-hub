# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 02: Usando Tipos de Dados e Conversões Corretamente**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Identificar e selecionar corretamente os **Tipos de Dados Primitivos** do ABAP (I, P, C, STRING, D, T), compreendendo suas implicações de memória e precisão.  
2. Diferenciar com clareza a **Conversão Implícita** (Automática) da **Conversão Explícita** (Manual), reconhecendo os riscos de confiar na "mágica" do compilador.  
3. Utilizar o operador construtor **CONV** para realizar conversões seguras, legíveis e com tratamento de exceções.  
4. Prevenir e diagnosticar erros comuns de **Overflow** (Estouro numérico) e **Truncation** (Corte de texto) que podem causar Dumps em produção.

### **1\. O Zoológico de Tipos ABAP: Uma Escolha Estratégica**

O ABAP possui um legado de tipos de dados que evoluiu ao longo de décadas. Escolher o tipo errado não é apenas uma questão de "funcionar ou não", mas de performance e integridade de dados financeiros.

#### **Numéricos: Precisão vs. Performance**

* **I (Integer):** Números inteiros de 4 bytes (ex: \-2.147.483.648 a \+2.147.483.647).  
  * *Uso Ideal:* Contadores de loops, índices de tabelas, quantidades indivisíveis (peças).  
  * *Risco:* Arredonda automaticamente qualquer decimal atribuído (ex: 3.9 vira 4).  
* **P (Packed Number):** Números de ponto fixo decimais. Armazena dois dígitos decimais por byte.  
  * *Uso Ideal:* **Obrigatório para valores monetários, pesos e medidas**.  
  * *Característica:* Garante precisão exata. Você define o tamanho total e as casas decimais (ex: TYPE p LENGTH 8 DECIMALS 2).  
  * *Atenção:* Cálculos intermediários podem estourar se a variável de destino for pequena.  
* **F (Binary Float):** Ponto flutuante binário (padrão IEEE 754).  
  * *Uso Ideal:* Cálculos científicos ou estatísticos que exigem magnitudes extremas (muito grande ou muito pequeno).  
  * *Risco:* **Impreciso para dinheiro**. O valor 0.1 pode ser armazenado como 0.09999999. Nunca use para saldo bancário.

#### **Texto: Fixo vs. Dinâmico**

* **C (Char):** Texto de tamanho fixo pré-alocado na memória.  
  * *Comportamento:* Se você declarar CHAR(10) e escrever "Oi", o sistema guarda "Oi " (com 8 espaços).  
  * *Uso:* Campos de código (Chaves, IDs, Status).  
* **STRING:** Texto de tamanho dinâmico.  
  * *Comportamento:* Aloca apenas o necessário mais um ponteiro de gerenciamento.  
  * *Uso:* Descrições, logs, concatenações longas. É o padrão do ABAP moderno.

#### **Data e Hora: O Formato Interno**

* **D (Date):** Tecnicamente é um CHAR(8) com formato AAAAMMDD.  
  * *Dica:* Permite aritmética de dias direta (data \+ 1).  
* **T (Time):** Tecnicamente é um CHAR(6) com formato HHMMSS.  
  * *Dica:* Permite aritmética de segundos (hora \+ 60).

### **2\. O Perigo da Conversão Implícita**

O ABAP é conhecido por ser "user-friendly" (ou permissivo demais) com tipos. Se você tentar colocar um texto num número, o compilador não dá erro de sintaxe; ele tenta converter em tempo de execução.

**Exemplo Perigoso:**

DATA: lv\_num TYPE i.  
lv\_num \= '100'.   " Funciona (converte string '100' para int 100\)  
lv\_num \= '100.9'. " Funciona (arredonda para 101\)  
lv\_num \= '100,9'. " ERRO\! (Depende da configuração do usuário: vírgula vs ponto)

**Os Riscos Principais:**

1. Truncation (Corte Silencioso):  
   Mover um texto longo para um curto corta o final sem avisar.  
   DATA: lv\_short TYPE c LENGTH 3\.  
   lv\_short \= 'Bananas'. " Resultado: 'Ban'. O resto sumiu.

2. Overflow (Estouro Numérico):  
   Mover um número grande para uma variável pequena causa um Dump (Erro em tempo de execução que derruba o programa).  
   * *Exceção:* CX\_SY\_CONVERSION\_OVERFLOW.  
3. Erro de Formato (Conversão Impossível):  
   Tentar converter 'ABC' para Inteiro.  
   * *Exceção:* CX\_SY\_CONVERSION\_NO\_NUMBER.

### **3\. A Solução Moderna: Operador CONV**

Para escrever código robusto (Clean Code), evite depender da sorte. Use **Conversão Explícita**. O operador CONV deixa claro para quem lê o código que uma transformação está ocorrendo e permite controlar o tipo de destino.

**Vantagens do CONV:**

* **Legibilidade:** Fica óbvio que String está virando Float.  
* **Inline:** Pode ser usado dentro de chamadas de método ou concatenações.  
* **Segurança:** Facilita o envelopamento em blocos TRY...CATCH.

" Exemplo: Chamando um método que espera um Inteiro, mas tenho um Char  
lo\_calc-\>somar(   
  iv\_val1 \= CONV i( '10' )   
  iv\_val2 \= 20   
).

" Exemplo: Garantindo precisão decimal ao ler de uma string  
DATA(lv\_price) \= CONV kbetr( '150.55' ).

### **4\. Conversão de Tipos Especiais (Alpha Conversion)**

No ecossistema SAP, muitos identificadores numéricos (Cliente, Fornecedor, Material, Pedido) são tecnicamente armazenados como texto (CHAR ou NUMC) e exigem preenchimento com zeros à esquerda para funcionarem nas buscas de banco de dados.

* **Externo (Tela):** 123  
* **Interno (Banco):** 0000000123

Para converter entre esses formatos sem ter que escrever loops manuais de concatenação, usamos o operador de formatação ALPHA.

DATA(lv\_input\_user) \= '123'.

" 1\. Preparar para o Banco (IN \= Colocar Zeros)  
DATA(lv\_matnr\_db) \= |{ lv\_input\_user ALPHA \= IN }|.   
" Resultado: '0000000123' (Agora o SELECT funciona)

" 2\. Preparar para a Tela (OUT \= Tirar Zeros)  
DATA(lv\_display) \= |{ lv\_matnr\_db ALPHA \= OUT }|.  
" Resultado: '123' (Mais amigável para o usuário)

### **5\. Exemplo Prático: Lidando com Conversões Robustas**

Neste exemplo, vamos simular o processamento de um arquivo texto (onde tudo é string) e a conversão segura para tipos de negócio, tratando erros de formato e internacionalização.

CLASS zcl\_data\_types DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_data\_types IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " \--- Cenário 1: Conversão Segura de Valor Monetário (String \-\> Packed) \---  
    " Imagine que este valor veio de uma API JSON ou arquivo CSV  
    DATA(lv\_string\_num) \= '150.55'.   
      
    TRY.  
        " Tenta converter String para Packed Number (Dinheiro)  
        " kbetr é um elemento de dados standard para valores  
        DATA(lv\_amount) \= CONV kbetr( lv\_string\_num ).  
        out-\>write( |Sucesso\! Valor convertido: { lv\_amount }| ).

    CATCH cx\_sy\_conversion\_no\_number.  
        out-\>write( 'Erro Crítico: O texto fornecido não é um número válido.' ).  
    ENDTRY.

    " \--- Cenário 2: Perda de Precisão (O Perigo do Arredondamento) \---  
    " O sistema converte automaticamente, mas arredonda. Isso pode gerar  
    " diferenças de centavos em relatórios financeiros.  
    DATA(lv\_float\_val) \= '10.7'.  
    DATA(lv\_int\_val)   \= CONV i( lv\_float\_val ).   
      
    out-\>write( |Atenção: Float { lv\_float\_val } virou Inteiro { lv\_int\_val } (Arredondou\!)| ).

    " \--- Cenário 3: Truncamento de Texto (Perda de Dados) \---  
    DATA(lv\_long\_text) \= 'Esta é uma frase muito longa que não cabe no destino'.  
    TYPES ty\_short\_char TYPE c LENGTH 10\.  
      
    " Ao forçar a conversão para um tipo menor, o ABAP corta o final  
    DATA(lv\_short) \= CONV ty\_short\_char( lv\_long\_text ).  
      
    out-\>write( |Texto original: { lv\_long\_text }| ).  
    out-\>write( |Texto cortado : '{ lv\_short }'| ). " Resultado: 'Esta é uma'

    " \--- Cenário 4: Conversão Alpha para Chaves \---  
    DATA(lv\_customer\_input) \= '4500'.  
      
    " Simulando busca no banco  
    " Se buscássemos WHERE kunnr \= '4500', falharia. O banco tem '0000004500'.  
    DATA(lv\_customer\_db) \= |{ lv\_customer\_input ALPHA \= IN }|.  
      
    out-\>write( |Input Usuário: { lv\_customer\_input } \-\> Chave Banco: { lv\_customer\_db }| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Type Casting (Conversão de Tipo):** Processo de transformar o valor de uma variável de um tipo de dado (ex: String) para outro (ex: Inteiro). Pode ser Implícito (feito automaticamente pelo compilador) ou Explícito (ordenado pelo desenvolvedor).  
* **Implicit Conversion (Conversão Implícita):** Quando o ABAP converte automaticamente os tipos na atribuição (=). É conveniente para prototipagem, mas perigoso em produção pois esconde arredondamentos e perdas de precisão.  
* **Explicit Conversion (CONV):** Uso do operador construtor CONV para forçar a conversão de forma clara. É a prática recomendada no ABAP Moderno para garantir a legibilidade e a intenção do código.  
* **Overflow (Estouro):** Erro de tempo de execução (Dump) que ocorre quando um número é grande demais para caber na variável de destino (Exceção: CX\_SY\_CONVERSION\_OVERFLOW).  
* **Truncation (Truncamento):** Perda de dados que ocorre quando uma string longa é movida para uma variável de texto mais curta. O final da string é descartado silenciosamente.  
* **Alpha Conversion:** Rotina padrão do SAP (CONVERSION\_EXIT\_ALPHA) usada para adicionar zeros à esquerda (IN) ou remover zeros (OUT) em campos numéricos armazenados como caracteres (ex: 000123).

#### **Regras de Ouro para Tipos**

| Cenário | Tipo Recomendado | Tipo a Evitar | Por quê? |
| :---- | :---- | :---- | :---- |
| **Dinheiro** | Packed (P) | Float (F) | Float tem imprecisão binária (0.1 virou 0.0999). |
| **Contadores** | Integer (I) | Packed (P) | Inteiros são processados mais rápido pela CPU. |
| **Texto** | String | Char (C) | String economiza memória e evita truncamento. |
| **IDs (Mat/Cli)** | Char/Numc | Integer | IDs podem ter zeros à esquerda que Inteiro remove. |

### **📝 Quiz de Fixação**

Q1: O que acontece se eu tentar converter a string 'ABC' para um tipo Inteiro (I) usando o operador CONV?  
R: Ocorrerá um erro em tempo de execução (Dump) do tipo CX\_SY\_CONVERSION\_NO\_NUMBER. Letras não podem ser convertidas matematicamente para números inteiros. Em código produtivo, devemos sempre envolver conversões de fontes externas em blocos TRY...CATCH.  
Q2: Qual a diferença fundamental de comportamento entre os tipos C (Char) e STRING?  
R: O tipo C tem tamanho fixo (alocado estaticamente). Se o texto for menor que o tamanho, ele preenche o resto com espaços em branco à direita. O tipo STRING tem tamanho dinâmico e cresce conforme necessário, economizando memória para textos curtos e permitindo textos muito longos sem corte.  
Q3: Como devo converter o valor '100' para o formato '0000000100' (formato padrão de banco de dados para materiais e clientes) usando String Templates?  
R: Utilizando a opção de formatação ALPHA \= IN. A sintaxe correta dentro do template é: |{ '100' ALPHA \= IN }|. Isso instrui o sistema a aplicar a rotina de conversão Alpha para adicionar os zeros à esquerda.