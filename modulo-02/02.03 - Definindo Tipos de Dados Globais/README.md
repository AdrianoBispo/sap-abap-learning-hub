# **Módulo 02: Modelagem de Dados com ABAP Dictionary e CDS**

## **Aula 03: Definindo Tipos de Dados Globais**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Explicar em profundidade a hierarquia de duas camadas do dicionário ABAP: **Domínio** (Definição Técnica) vs **Elemento de Dados** (Definição Semântica), e como esse desacoplamento favorece a manutenção.  
2. Criar e configurar Elementos de Dados para garantir a **tradução automática** e a seleção inteligente de labels (Rótulos) nas aplicações Fiori responsivas.  
3. Entender o papel crítico dos **Value Helps** (Ajudas de Pesquisa) e **Rotinas de Conversão** associados aos tipos globais.  
4. Dominar o uso de **Enumerations** (Enumerações) como a alternativa moderna e type-safe para listas de valores fixos.

### **1. A Hierarquia de Tipos do ABAP Dictionary**

Para criar um campo reutilizável e consistente em milhares de tabelas e programas, o SAP utiliza uma arquitetura robusta de metadados dividida em duas camadas. Isso promove o princípio de "Single Source of Truth" (Fonte Única da Verdade) para definições de dados.

#### **Camada 1: Domínio (Domain) - "O Técnico"**

O Domínio é a base da pirâmide. Ele define as propriedades técnicas puras do dado, independentemente do seu significado de negócio. Vários Elementos de Dados diferentes (ex: "Telefone Comercial", "Telefone Celular", "Fax") podem apontar para o mesmo Domínio técnico (ex: CHAR30).

* **Tipo de Dado e Tamanho:** Define o formato físico no banco (CHAR, DEC, INT4, DATS, TIMS) e o comprimento (Length).  
* **Propriedades de Saída:** Define se o campo permite minúsculas (Lower Case) ou se requer sinal negativo.  
* **Rotina de Conversão (Conversion Routine):** Um recurso vital. Define uma função que é executada automaticamente ao mover dados da tela para o banco (INPUT) e vice-versa (OUTPUT).  
  * *Exemplo:* A rotina ALPHA converte automaticamente 123 para 0000000123 (Input) e remove os zeros na exibição (Output). Sem isso, as chaves numéricas do SAP seriam ingovernáveis.  
* **Valores Fixos (Fixed Values):** Uma lista estática de valores válidos permitidos para aquele domínio (ex: 'A' = Ativo, 'C' = Cancelado). Isso gera uma validação automática em telas clássicas, mas no modelo RAP preferimos CDS Views de domínio ou Enumerações.

#### **Camada 2: Elemento de Dados (Data Element) - "O Semântico"**

O Elemento de Dados adiciona significado e contexto ao Domínio. Ele responde à pergunta: "O que esse dado representa para o usuário?".

* **Rótulos de Campo (Field Labels):** Este é o segredo da UI do SAP. Você define quatro variantes de texto para o mesmo campo:  
  * *Short (10):* "Status"  
  * *Medium (20):* "Status do Pedido"  
  * *Long (40):* "Status Atual do Processamento"  
  * *Heading:* "St." (Para cabeçalhos de colunas estreitas)  
  * *O Mágica do Fiori:* O SAP Fiori Elements é responsivo. Se a coluna na tela for larga, ele usa o rótulo "Long". Se o usuário acessar pelo celular e a tela encolher, o Fiori troca automaticamente para "Medium" ou "Short". Se você não preencher isso corretamente, a UI ficará quebrada em telas pequenas.  
* **Ajuda F1 (Documentation):** Você pode escrever um texto explicativo técnico ou de negócio que aparecerá quando o usuário pedir ajuda sobre o campo.  
* **Parameter ID (SET/GET):** Permite que o campo lembre o último valor digitado pelo usuário entre diferentes transações (memória da sessão SAP).

### **2. Por que criar Elementos de Dados Customizados?**

No desenvolvimento RAP ágil, pode ser tentador definir um campo na tabela apenas como abap.char(20) (Tipo Primitivo/Built-in). Por que investir tempo criando objetos no Dicionário?

1. O Problema da UI e Tradução:  
Se você usar tipos primitivos, a coluna no aplicativo Fiori não terá um texto mestre. Ela aparecerá com o nome técnico do campo (ex: Z_USER_STATUS) ou vazia.

* *Consequência:* Você terá que redefinir o rótulo manualmente (@EndUserText.label) em cada uma das 10 CDS Views que usam esse campo.  
* *Tradução:* Se sua empresa tem filiais globais, você terá que traduzir esses labels em cada view. Com um Elemento de Dados, você traduz uma vez (via SE63 ou ADT) e o sistema propaga a tradução para todas as telas automaticamente.

2. Análise de Impacto (Where-Used List):  
Ao usar um Elemento de Dados global (Z_CUSTOMER_ID), você pode usar a ferramenta "Where-Used List" para encontrar todas as tabelas, estruturas, classes e métodos que utilizam esse conceito de negócio. Com tipos primitivos, essa rastreabilidade semântica se perde.  
**A Solução:** Ao usar um Elemento de Dados z_status_xyz com o label "Status do Pedido", o Fiori automaticamente exibe "Status do Pedido" na tela, em qualquer idioma que o usuário logar, garantindo consistência e reduzindo esforço de manutenção.

### **3. Enumerações (Enums): A Abordagem Moderna**

Introduzidas no ABAP 7.51+, as **Enumerações** representam uma evolução significativa em relação aos antigos "Valores Fixos de Domínio". Elas permitem definir um conjunto fixo de constantes tipadas que oferecem **Type Safety** (Segurança de Tipo).

Diferente de constantes globais simples, uma variável tipada com um Enum **só pode aceitar** os valores definidos naquele Enum. O compilador protege o código contra atribuições inválidas.

**Exemplo de Definição (Código):**

INTERFACE if_status_enums PUBLIC.  
  " Define um tipo ENUM que, por baixo dos panos, é um CHAR1  
  TYPES:  
    BEGIN OF ENUM ty_status STRUCTURE status BASE TYPE char1,  
      initial   VALUE IS INITIAL, " Valor ''  
      created   VALUE 'C',        " Valor 'C'  
      processed VALUE 'P',        " Valor 'P'  
      rejected  VALUE 'R',        " Valor 'R'  
    END OF ENUM ty_status STRUCTURE status.  
ENDINTERFACE.

**Uso no Código (Type Safety):**

DATA: lv_status TYPE if_status_enums=>ty_status.

" Atribuição válida  
lv_status = if_status_enums=>status-rejected.

" Comparação legível  
IF lv_status = if_status_enums=>status-rejected.  
  " Lógica clara e legível, sem "Magic Strings" espalhadas  
ENDIF.

" Isso geraria ERRO de sintaxe, protegendo o código:  
" lv_status = 'X'. " 'X' não é um valor válido do Enum!

### **4. Prática no ADT (Eclipse)**

Diferente das tabelas (que usam código fonte DEFINE TABLE), a criação de Domínios e Elementos de Dados no ADT ainda utiliza editores baseados em formulários (Form-Based Editors), mas eles estão totalmente integrados ao fluxo de projeto e ao Git.

1. **Criação:** Botão Direito no Pacote > **New** > **Dictionary** > **Data Element**.  
2. **Definição de Tipo:** Na aba principal, escolha entre "Built-in Type" (para tipos simples sem domínio, mas com labels) ou "Domain" (para reutilização técnica máxima).  
3. **Labels (Crucial):** Vá na aba **"Field Labels"**. Preencha Short (10), Medium (20), Long (40) e Heading.  
   * *Dica:* Use textos que façam sentido isoladamente. Evite abreviações obscuras no label "Long".  
4. **Ativação:** Salve e ative (Ctrl+F3). O objeto agora está disponível para uso em tabelas e CDS Views.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Domain (Domínio):** Objeto do dicionário que define os atributos técnicos (tipo, comprimento, casas decimais) e regras de transformação (Rotinas de Conversão) de um campo. É a menor unidade de definição técnica.  
* **Data Element (Elemento de Dados):** Objeto do dicionário que descreve o significado semântico e de negócio de um campo. Ele carrega os textos (Field Labels), a documentação (F1) e pode carregar Search Helps.  
* **Field Label (Rótulo de Campo):** Textos definidos dentro do Elemento de Dados (Curto, Médio, Longo, Cabeçalho). O SAP Fiori Elements utiliza uma lógica inteligente para escolher qual label exibir dependendo do espaço disponível na tela (Responsividade).  
* **Conversion Routine (Rotina de Conversão):** Um par de Function Modules (Input/Output) associado a um Domínio que transforma o dado automaticamente (ex: Formato interno de banco vs. Formato externo de visualização).  
* **Enumeration (Enum):** Tipo de dados especial que define um conjunto estrito de constantes nomeadas. Melhora a legibilidade do código ABAP e previne erros de atribuição, garantindo que uma variável contenha apenas valores válidos do conjunto.

#### **Relação Hierárquica: Tabela x Elemento x Domínio**

Imagine um campo de "Número de Telefone Comercial":

1. **Tabela:** ZCLIENTE-TEL_COMERCIAL (O campo físico na tabela).  
2. **Elemento de Dados:** Z_TELEFONE_COMERCIAL (Label: "Tel. Comercial", "Telefone Comercial do Cliente").  
3. **Domínio:** Z_CHAR30 (Técnico: CHAR de 30 posições, sem distinção de maiúsculas/minúsculas). *Nota: O mesmo domínio pode ser usado para o Elemento de Dados "Telefone Residencial".*

### **📝 Quiz de Fixação**

Q1: Se eu criar uma tabela e definir um campo diretamente como abap.char(10) sem usar um Elemento de Dados, qual será a consequência na aplicação Fiori Elements gerada sobre essa tabela?  
R: O campo aparecerá na interface sem um rótulo amigável (provavelmente mostrando o nome técnico da coluna, como MY_FIELD, ou vazio), pois o Fiori busca os textos de exibição nos "Field Labels" do Elemento de Dados. Além disso, a tradução do rótulo terá que ser feita manualmente em cada aplicação que consumir essa tabela.  
Q2: Qual é a principal diferença de responsabilidade entre um Domínio e um Elemento de Dados?  
R: O Domínio define a parte técnica (tipo físico, tamanho, rotinas de conversão), focando em como o dado é armazenado. O Elemento de Dados define a parte semântica (significado de negócio, labels de tela, ajuda F1), focando em o que o dado representa para o usuário.  
Q3: As Enumerações (Enums) são objetos globais do dicionário criados via SE11?  
R: Não exatamente da forma clássica (como tabelas). Embora existam Enumerações globais no Dicionário (a partir do ABAP 7.51), elas são definidas através de código fonte em Interfaces ou Classes Globais (ou artefatos específicos de Type Group modernos) para uso em lógica de programação, oferecendo tipagem forte (Type Safety) que as antigas listas de valores fixos de domínio não possuíam.