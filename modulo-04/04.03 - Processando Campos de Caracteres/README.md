# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 03: Processando Campos de Caracteres**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Substituir comandos de manipulação de string obsoletos (TRANSLATE, SEARCH, SPLIT) por **Funções de String Embutidas** (to_upper, substring, condense, replace), aproveitando o encadeamento de métodos.  
2. Aplicar **Expressões Regulares (Regex)** para resolver problemas complexos de validação (e-mail, CPF, telefone) e extração de padrões que seriam impossíveis com comandos simples.  
3. Utilizar funções predicativas como **contains**, **matches** e **starts_with** diretamente em condições lógicas, eliminando variáveis auxiliares e melhorando a legibilidade.  
4. Realizar concatenações e formatações avançadas com **String Templates**, integrando lógica de apresentação diretamente na construção da string.

### **1. Funções Embutidas: O Fim do "TRANSLATE"**

No ABAP clássico, a manipulação de strings era feita através de comandos imperativos que modificavam a variável no local. Isso impedia o encadeamento de operações e tornava o código verboso. O ABAP 7.40+ introduziu uma biblioteca rica de funções que retornam o resultado, permitindo um estilo de programação funcional.

#### **Maiúsculas e Minúsculas**

* **Antigo:** TRANSLATE lv_text TO UPPER CASE. (Modifica lv_text destrutivamente).  
* **Moderno:** lv_text = to_upper( lv_text ). (Retorna uma nova string, preservando a original se desejado).  
  * *Variação:* to_lower( ) e to_mixed( ) (CamelCase).

#### **Substrings e Tamanho**

* **Antigo:** lv_part = lv_text+0(5). (Sintaxe de Offset/Length). É rígida e causa dumps se o offset for maior que o tamanho da string.  
* **Moderno:** lv_part = substring( val = lv_text off = 0 len = 5 ).  
  * *Vantagem:* Mais seguro e legível. Permite usar expressões para calcular o offset dinamicamente.

#### **Limpeza de Espaços**

* **Antigo:** CONDENSE lv_text NO-GAPS.  
* **Moderno:** lv_text = condense( val = lv_text from = ' ' ).  
  * *Diferença:* A função condense() moderna remove espaços das pontas (Trim) e reduz espaços múltiplos internos para um único espaço. Para remover *todos* os espaços (como o NO-GAPS), você usaria replace ou especificaria argumentos adicionais.

### **2. O Poder das Expressões Regulares (Regex)**

Quando precisamos encontrar ou validar padrões abstratos (e não apenas texto fixo), as funções de string tradicionais falham. O suporte a Regex no ABAP moderno é robusto e segue o padrão POSIX/PCRE.

#### **Validação (Matches)**

Verificar se um texto segue um formato específico, como um e-mail corporativo.

" Padrão: Texto + @ + Texto + . + Texto (Simplificado)  
IF matches( val = lv_email regex = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+.[a-zA-Z]{2,}$' ).  
  " É um email sintaticamente válido  
ELSE.  
  " Formato inválido  
ENDIF.

#### **Substituição e Limpeza (Replace)**

Um cenário comum em integrações é receber dados "sujos", como um telefone (11) 99999-0000 que precisa ser salvo no banco apenas como números 11999990000.

DATA(lv_phone) = '(11) 99999-0000'.

" Regex '[^0-9]' significa: Encontre qualquer caractere que NÃO (^) seja um dígito (0-9).  
" A função substitui esses caracteres por vazio (''), limpando a string.  
DATA(lv_clean) = replace( val = lv_phone regex = '[^0-9]' with = '' occ = 0 ).  
" Resultado: '11999990000'

### **3. Funções Predicativas**

São funções especiais projetadas para retornar um valor booleano implícito, permitindo seu uso direto dentro de comandos IF, CHECK ou COND. Elas eliminam a necessidade de fazer uma operação, checar o sy-subrc e depois decidir.

* **contains( val = ... sub = ... )**: Verifica se uma string contém uma substring.  
* **starts_with / ends_with**: Verifica se a string começa ou termina com determinado padrão.  
* **matches**: Verifica se a string corresponde a um Regex.

" Verifica se é uma mensagem de erro sem precisar de SEARCH  
IF contains( val = lv_msg sub = 'Erro' ) OR starts_with( val = lv_msg sub = 'E:' ).  
  " Tratar erro  
ENDIF.

### **4. Exemplo Prático: Higienização de Dados**

Vamos criar uma classe utilitária que recebe dados brutos de um cadastro externo (ex: planilha Excel ou API legado) e os padroniza para o formato SAP.

CLASS zcl_string_proc DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if_oo_adt_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl_string_proc IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Dados de Entrada (Sujos e despadronizados)  
    DATA(lv_raw_name)  = '  joão  da  silva  '.  
    DATA(lv_raw_email) = 'JOAO.SILVA@SAP.COM'.  
    DATA(lv_raw_sku)   = 'MAT-1234-BR'.

    " 1. Padronização de Nome (Limpeza e Maiúsculas)  
    " Encadeamento: Primeiro remove espaços extras, depois converte para maiúsculo  
      
    DATA(lv_name) = to_upper( condense( val = lv_raw_name ) ).  
      
    out->write( |Nome Limpo: '{ lv_name }'| ).

    " 2. Padronização de Email (Minúsculo)  
    " E-mails são geralmente armazenados em minúsculo para facilitar busca/login  
    DATA(lv_email) = to_lower( lv_raw_email ).  
    out->write( |Email Limpo: '{ lv_email }'| ).

    " 3. Extração de Informação com Regex  
    " Cenário: Extrair apenas a parte numérica do SKU 'MAT-1234-BR'  
    " O padrão d+ busca uma sequência contínua de dígitos.  
      
    find( val = lv_raw_sku regex = 'd+' sub = DATA(lv_extracted_num) ).  
      
    out->write( |Número do Material extraído: { lv_extracted_num }| ).

    " 4. Validação Lógica com Predicados  
    " Classificar o produto baseado no prefixo  
      
    DATA(lv_type) = COND string(  
      WHEN starts_with( val = lv_raw_sku sub = 'MAT' ) THEN 'Material Físico'  
      WHEN starts_with( val = lv_raw_sku sub = 'SRV' ) THEN 'Serviço'  
      ELSE 'Desconhecido'  
    ).  
      
    out->write( |Tipo de Produto: { lv_type }| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Built-in Functions (Funções Embutidas):** Funções nativas da linguagem ABAP (como strlen, to_upper, replace) que podem ser usadas em qualquer posição de operando (dentro de IFs, atribuições, chamadas de método), substituindo comandos procedurais antigos.  
* **Regex (Regular Expressions):** Uma linguagem formal para descrever padrões de texto. Usada para validação complexa (ex: formatos de CPF, CNPJ, E-mail) e operações de busca/substituição avançadas que variam conforme o conteúdo.  
* **Chaining (Encadeamento):** A capacidade de chamar uma função dentro do resultado de outra, como func1( func2( var ) ). Isso permite escrever transformações complexas de dados em uma única linha legível.  
* **Predicative Function:** Uma função que retorna um valor verdade (booleano) para uso em expressões lógicas. Exemplos: contains, matches, line_exists.

#### **Tabela Comparativa: Antigo vs Novo**

| Operação | Comando Legado (Evitar) | Função Moderna (Usar) |
| :---- | :---- | :---- |
| **Maiúsculas** | TRANSLATE x TO UPPER CASE | x = to_upper( x ) |
| **Tamanho** | Variável de sistema ou STRLEN | strlen( x ) |
| **Busca** | SEARCH x FOR 'ABC' | find( val = x sub = 'ABC' ) |
| **Confere Padrão** | CP (Contains Pattern) | matches( val = x regex = ... ) |
| **Concatenar** | CONCATENATE a b INTO c | c = |{ a }{ b }| |
| **Substituir** | REPLACE 'A' WITH 'B' INTO x | x = replace( val = x sub = 'A' with = 'B' ) |

### **📝 Quiz de Fixação**

Q1: Qual a diferença de comportamento entre a função condense( val = text ) e o comando antigo CONDENSE text NO-GAPS?  
R: A função condense() moderna, por padrão, remove apenas os espaços em branco das extremidades (trim) e reduz múltiplos espaços internos para um único espaço (ex: "A B" vira "A B"). O comando NO-GAPS remove todos os espaços da string, colando os caracteres (ex: "A B" vira "AB"). Para simular NO-GAPS modernamente, usa-se replace ou argumentos específicos.  
Q2: Para verificar se uma variável de string contém o domínio "@sap.com" no final (sufixo), qual função é a mais performática e legível?  
R: A função predicativa ends_with( val = lv_email sub = '@sap.com' ). Ela é semanticamente clara e otimizada para essa verificação específica.  
Q3: O que a expressão regex [^0-9] significa quando usada em uma função replace para limpeza de dados?  
R: Ela significa "Qualquer caractere que NÃO seja um dígito numérico de 0 a 9". O acento circunflexo ^ dentro dos colchetes [] nega o conjunto. É comumente usada para higienizar strings que deveriam ser numéricas (como telefones ou documentos), removendo formatação como pontos, traços e parênteses.