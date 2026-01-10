# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 09: Adicionando Documentação ao Código ABAP (ABAP Doc)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar com clareza **Comentários Tradicionais** (" ou *), destinados a anotações internas de implementação, de **ABAP Doc** ("!), destinado à documentação de interface pública e contratos de API.  
2. Dominar o uso de tags de documentação estruturada como **@parameter**, **@raising**, **@returning** e o uso de **Tags HTML** para formatação rica de texto.  
3. Utilizar ferramentas da IDE (ADT) como a janela **Element Info (F2)** para consumir a documentação em tempo de codificação, aumentando a produtividade.  
4. Aplicar o princípio de **Clean Code** na documentação: entender que o código deve ser autoexplicativo quanto ao "como", enquanto a documentação deve focar no "o quê" e no "porquê", servindo como um manual de uso para outros desenvolvedores.

### **1. Adeus Comentários de Linha, Olá ABAP Doc**

Durante décadas, a documentação no SAP foi tratada de forma secundária, muitas vezes consistindo em blocos de texto ASCII ("flower boxes") no cabeçalho de Reports ou Function Modules.

**O Estilo Antigo:**

************************************************************************  
* Método: CALC_TAX  
* Autor: João Silva  
* Data: 20/10/2010  
* Descrição: Calcula imposto. Parâmetro IV_VAL é o valor total.  
************************************************************************  
METHOD calc_tax.

**O Problema:** Esse texto é invisível para a IDE. É apenas "ruído" visual que o compilador e as ferramentas de desenvolvimento ignoram. Se outro desenvolvedor chamar seu método em outra classe, ele não verá essas instruções a menos que abra o código-fonte da sua classe e role até a definição. Isso quebra o encapsulamento e diminui a produtividade.

**A Solução (ABAP Doc):** Introduzido para alinhar o ABAP com linguagens modernas como Java (Javadoc) e C# (XML Doc), o **ABAP Doc** usa uma sintaxe especial ("!) posicionada antes da definição do artefato. O ADT lê, interpreta e formata esse conteúdo, apresentando-o como uma janela de ajuda flutuante e integrada sempre que o artefato é referenciado. Isso transforma seu código em uma API autodocumentada.

### **2. Sintaxe, Tags e Formatação Rica**

O ABAP Doc deve ser escrito na **Definição** da classe (ou Interface), pois é lá que reside o "contrato" público. Comentários na Implementação são invisíveis para quem consome a classe.

#### **Sintaxe Básica**

* **"!**: Inicia uma linha de ABAP Doc. Deve ser colocado imediatamente antes da declaração do método, atributo ou classe.

#### **Tags de Parâmetros**

Estas tags estruturam a documentação dos elementos da assinatura do método:

* **@parameter**: Descreve um parâmetro de IMPORTING, EXPORTING ou CHANGING.  
  * *Sintaxe:* "! @parameter nome_param | Descrição do que ele faz.  
* **@returning**: Descreve o valor de retorno funcional.  
* **@raising**: Descreve as exceções que podem ocorrer, explicando *sob quais condições* o erro é disparado.

#### **Formatação com HTML**

Diferente dos comentários antigos, o ABAP Doc suporta um subconjunto de tags HTML para tornar a leitura mais agradável e organizada:

* **<p>**: Parágrafos.  
* **<em>** ou **<i>**: Itálico (Ênfase).  
* **<strong>** ou **<b>**: Negrito (Destaque).  
* **<code>**: Formata como código (fonte monoespaçada).  
* **<ul>, <ol>, <li>**: Listas não ordenadas e ordenadas.

**Exemplo Completo:**

"! <p class="shorttext">Calcula o valor do desconto de fidelidade progressivo.</p>  
"! O cálculo é baseado no <em>histórico de compras</em> do cliente nos últimos 12 meses.  
"! A lógica segue a tabela de descontos definida na política comercial <strong>ZPOL_01</strong>.  
"!  
"! <ul>  
"!   <li>Categoria A: 10%</li>  
"!   <li>Categoria B: 5%</li>  
"! </ul>  
"!  
"! @parameter iv_customer_id | ID do Cliente (Tabela KNA1)  
"! @parameter iv_total_amount | Valor total da compra atual para base de cálculo  
"! @returning rv_discount | Valor monetário do desconto calculado em Moeda Local  
"! @raising zcx_customer_error | Disparado se o cliente estiver bloqueado ou não existir  
METHODS calculate_discount  
  IMPORTING iv_customer_id     TYPE kunnr  
            iv_total_amount    TYPE kbetr  
  RETURNING VALUE(rv_discount) TYPE kbetr  
  RAISING   zcx_customer_error.

### **3. A Janela "Element Info" (F2): Produtividade na IDE**

A grande vantagem do ABAP Doc não é a escrita, mas a **leitura**. A integração com a IDE (ADT/Eclipse) muda a forma como navegamos no código.

O Fluxo de Trabalho:  
Imagine que você está escrevendo um código que usa a classe de um colega. Você digita lo_calculadora-> e o Content Assist sugere o método calculate_discount.

1. **Sem ABAP Doc:** Você vê apenas o nome técnico dos parâmetros (IV_VAL). Você precisa parar, abrir a classe zcl_calculadora, ler o código para entender o que é IV_VAL e depois voltar.  
2. **Com ABAP Doc:** Você coloca o cursor sobre o método ou pressiona **F2**. Um pop-up amarelo (Element Info) aparece instantaneamente, mostrando:  
   * O texto explicativo formatado.  
   * A lista de parâmetros e suas descrições.  
   * As exceções possíveis.

Isso permite que o desenvolvedor entenda o **contrato de uso** sem precisar investigar a **implementação interna**, respeitando o princípio de encapsulamento e economizando tempo precioso.

### **4. Clean Code vs. Comentários: A Filosofia**

Existe um ditado famoso no desenvolvimento de software: *"Código bom não precisa de comentários"*. Isso é uma meia-verdade que precisa ser contextualizada.

* Comentários de "O Quê" (Redundantes):  
  Evite descrever o óbvio. O código já diz o que está acontecendo.  
  lv_count = lv_count + 1. " Incrementa contador (NÃO FAÇA ISSO! É ruído.)

* Comentários de "Por Que" (Necessários):  
  Use comentários tradicionais (") dentro da implementação para explicar decisões de design não óbvias, regras de negócio obscuras ou workarounds técnicos.  
  " Incrementamos em 1 para compensar o offset do array que começa em 0 no sistema legado

* Documentação de Contrato (ABAP Doc):  
  Use ABAP Doc ("!) nas interfaces públicas (Classes, Métodos, Interfaces). O objetivo aqui não é explicar como o código funciona linha a linha, mas sim ensinar como usar aquela classe.

**Regra de Ouro:**

1. Use nomes de métodos e variáveis longos e descritivos (calculate_total_tax em vez de calc).  
2. Use ABAP Doc para documentar a **intenção** e o **contrato** da API pública.  
3. Use comentários internos (") com parcimônia, apenas para explicar complexidades de negócio inusuais.

### **5. Exemplo Prático: Classe Bem Documentada**

Neste exemplo, veremos uma classe com documentação de nível profissional, utilizando formatação HTML para listas e explicações claras dos parâmetros.

"! <p class="shorttext">Serviço de verificação de disponibilidade de estoque.</p>  
"! Esta classe atua como uma fachada para o sistema ERP legado e para o cache local.  
"!  
"! <h3>Estratégia de Busca:</h3>  
"! <ol>  
"!   <li>Verifica o cache de memória local (Redis/Shared Memory).</li>  
"!   <li>Se não encontrar, consulta o ERP em tempo real.</li>  
"! </ol>  
CLASS zcl_doc_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.  
  PUBLIC SECTION.  
    
    "! <p class="shorttext">Verifica a disponibilidade de um material específico.</p>  
    "! Este método deve ser chamado antes de qualquer tentativa de reserva.  
    "! Caso o sistema externo esteja offline, ele retornará o último status conhecido do cache.  
    "!  
    "! @parameter iv_sku   | Código do material (Stock Keeping Unit). Ex: <em>'MAT-001'</em>  
    "! @parameter iv_plant | Centro logístico (ex: '1000'). Se vazio, busca em todos os centros.  
    "! @returning rv_available | Retorna <strong>abap_true</strong> se houver saldo > 0 disponível para promessa (ATP).  
    "! @raising zcx_material_not_found | Disparado se o SKU não existir no cadastro mestre.  
    METHODS check_availability  
      IMPORTING iv_sku              TYPE string  
                iv_plant            TYPE string OPTIONAL  
      RETURNING VALUE(rv_available) TYPE abap_bool  
      RAISING   zcx_material_not_found.

  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl_doc_demo IMPLEMENTATION.  
  METHOD check_availability.  
    " A implementação está oculta e pode mudar (ex: trocar de banco para API)  
    " sem que o consumidor precise saber, pois o contrato (ABAP Doc) permanece o mesmo.  
      
    " Lógica simulada  
    IF iv_sku IS INITIAL.  
      RAISE EXCEPTION NEW zcx_material_not_found( ).  
    ENDIF.  
      
    rv_available = abap_true.  
  ENDMETHOD.  
ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **ABAP Doc:** Padrão de documentação técnica embutida no código fonte ABAP, utilizando o prefixo "!. É interpretado pelo ADT para gerar documentação rica, formatada e integrada à IDE.  
* **Element Info (F2):** Funcionalidade do Eclipse/ADT que exibe um pop-up com informações detalhadas (assinatura, tipos, ABAP Doc) sobre o elemento sob o cursor, permitindo leitura rápida sem navegação.  
* **Javadoc:** Padrão de documentação da linguagem Java que inspirou o ABAP Doc. Baseia-se na extração de comentários estruturados para gerar manuais de API em HTML.  
* **Tag (@...):** Palavras-chave especiais (anotações) dentro do ABAP Doc usadas para estruturar semanticamente a documentação de parâmetros, retornos e exceções.  
* **API Contract (Contrato de API):** O conjunto de regras públicas (nomes de métodos, tipos de entrada/saída, exceções e o comportamento documentado) que uma classe oferece aos seus consumidores. O ABAP Doc formaliza esse contrato.

#### **Guia de Estilo: Onde Documentar?**

| Localização | Usar ABAP Doc ("!) | Usar Comentário (") | Motivo |
| :---- | :---- | :---- | :---- |
| **Definição de Método** | **Sim** (Obrigatório para APIs) | Não | Define o contrato público para o consumidor. |
| **Interfaces (ZIF)** | **Sim** | Não | A interface é pura definição; precisa explicar o propósito. |
| **Meio do Código** | Não | **Sim** (Opcional) | Explica o "porquê" de uma lógica complexa interna. |
| **Atributos Públicos** | **Sim** | Não | Explica o significado de uma variável global/constante. |

### **📝 Quiz de Fixação**

Q1: Qual é o caractere especial que identifica uma linha como sendo comentário ABAP Doc e o diferencia de um comentário de linha comum?  
R: O conjunto de caracteres "! (Aspas duplas seguidas de exclamação). Comentários normais usam apenas aspas duplas (") ou asterisco (*) na coluna 1.  
Q2: Onde devo escrever o ABAP Doc de um método: na seção de Definição (DEFINITION) ou na seção de Implementação (IMPLEMENTATION) da classe?  
R: Na DEFINITION. O ABAP Doc destina-se a quem vai consumir a classe (interface pública), portanto deve estar junto da assinatura do método. O ADT lê a documentação a partir da definição para exibir no pop-up F2.  
Q3: Além de texto simples, o que mais posso usar dentro de um bloco ABAP Doc para melhorar a legibilidade?  
R: Você pode usar Tags HTML básicas (como <p>, <ul>, <li>, <b>, <code>) para formatar o texto, criar listas e dar ênfase a partes importantes da explicação.  
Q4: Qual tag do ABAP Doc é utilizada para descrever as condições sob as quais um método pode lançar um erro?  
R: A tag @raising, seguida do nome da classe de exceção e da descrição do cenário de erro.