# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 05: Entendendo os Floorplans do SAP Fiori Elements**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Decompor a anatomia de um **List Report** (Relatório de Lista), identificando as nuances funcionais da Barra de Filtros Inteligente e as capacidades da Área de Conteúdo.  
2. Decompor a anatomia de uma **Object Page** (Página de Objeto), compreendendo a hierarquia visual entre Título, Cabeçalho Expansível, Seções e Facetas.  
3. Compreender o conceito de **Facetas (Facets)** como a unidade fundamental de construção de layout no Fiori Elements, atuando como containers lógicos de informação.  
4. Distinguir com precisão entre *Reference Facets* (Conteúdo) e *Collection Facets* (Estrutura), e como elas se combinam para criar layouts complexos e aninhados.

### **1. Anatomia do List Report (Relatório de Lista)**

O *List Report* é muito mais do que uma simples tabela de dados; é um painel de controle projetado para lidar com grandes volumes de informação. Ele permite que o usuário refine progressivamente sua busca até encontrar os registros de interesse para ação. A página é dividida em duas grandes áreas verticais com responsabilidades distintas.

#### **A. Cabeçalho de Filtros (Smart Filter Bar)**

Esta é a área de controle superior. Sua função principal é reduzir o universo de dados (Dataset) antes que ele seja exibido, garantindo performance e relevância.

* **Função:** Permitir que o usuário restrinja o conjunto de dados através de critérios múltiplos, suportando desde buscas exatas até intervalos complexos.  
* **Componentes:**  
  * **Barra de Pesquisa Básica (Search Field):** Um campo de texto livre que realiza uma busca "fuzzy" (aproximada) em várias colunas configuradas como pesquisáveis (@Search.defaultSearchElement). É o equivalente ao "Google Search" do seu aplicativo.  
  * **Campos de Seleção (Selection Fields):** Inputs estruturados configurados via @UI.selectionField. Eles suportam *Value Helps* (F4), intervalos de data e múltipla seleção.  
  * **Adapt Filters (Adaptar Filtros):** Um botão crucial que abre um diálogo permitindo ao usuário adicionar campos de filtro que não estão visíveis por padrão. Isso permite que a barra de filtros permaneça limpa ("Simple"), mas poderosa quando necessário.  
  * **Gerenciamento de Variantes (Variant Management):** Permite que o usuário salve uma combinação de filtros (ex: "Viagens Abertas do Cliente X") como uma visão favorita para acesso rápido futuro.

#### **B. Área de Conteúdo (Content Area)**

Abaixo da barra de filtros reside a área de resultados. Esta seção é altamente dinâmica e reage às ações do usuário no cabeçalho.

* **Função:** Exibir os resultados filtrados, oferecer ferramentas de análise (ordenação, agrupamento) e fornecer gatilhos para ações de negócio.  
* **Componentes:**  
  * **Abas (Tabs) / Múltiplas Visões:** (Opcional) Permite segmentar os dados em categorias pré-definidas (ex: uma aba para "Em Aberto", outra para "Aprovadas" e outra para "Rejeitadas"). Cada aba pode ter colunas e filtros diferentes, baseados em SelectionPresentationVariants.  
  * **Barra de Ferramentas da Tabela (Table Toolbar):** O centro de comando operacional. Contém:  
    * **Ações CRUD Padrão:** Botões como *Criar* e *Excluir*, controlados pelas capacidades do Behavior Definition (BDEF).  
    * **Ações Customizadas:** Botões específicos de negócio (ex: "Copiar Viagem", "Simular Preço") definidos via anotações @UI.lineItem com type #FOR_ACTION.  
    * **Ações de Sistema:** Botões para *Exportar para Excel*, *Configurações de Coluna* (personalização) e *Tela Cheia*.  
  * **Tabela (Smart Table):** O grid de dados propriamente dito. Configurada via @UI.lineItem, ela suporta renderização responsiva (colunas que somem em telas pequenas), navegação para detalhes (clique na linha) e indicadores visuais de status (Criticality).

### **2. Anatomia da Object Page (Página de Objeto)**

A *Object Page* é o coração transacional do aplicativo. Enquanto o List Report é sobre *encontrar*, a Object Page é sobre *entender e modificar*. Ela exibe os detalhes profundos de uma única entidade e suas relações. Sua estrutura é desenhada para guiar o olhar do usuário do mais importante (Resumo) para o mais detalhado.

#### **A. Título do Cabeçalho (Header Title)**

A faixa superior fixa que permanece visível mesmo quando a página rola. Ela fornece o contexto imediato "Onde estou?".

* **Conteúdo:** Mostra o **Título do Objeto** (ex: "Viagem 001234") e informações críticas de resumo, como o Status do documento ("Confirmado", "Pendente").  
* **Ações Globais:** Botões que afetam o objeto inteiro, como *Editar*, *Deletar* ou ações de fluxo de trabalho como *Aprovar/Rejeitar*.  
* **Configuração:** Controlado principalmente pela anotação @UI.headerInfo.

#### **B. Conteúdo do Cabeçalho (Header Content)**

Uma área expansível logo abaixo do título, usada para KPIs e métricas que ajudam na tomada de decisão rápida sem precisar ler o formulário inteiro.

* **Conteúdo:** Exibe *Datapoints* (Pontos de Dados), *Micro Charts* (Gráficos em miniatura), links de contato ou etiquetas de prioridade. Exemplo: "Valor Total: R$ 5.000,00", "Progresso: 75%".  
* **Comportamento:** Esta área pode ser colapsada (escondida) pelo usuário para ganhar espaço de tela para o corpo da página.  
* **Configuração:** Utiliza a anotação @UI.facet com a propriedade purpose: #HEADER.

#### **C. Corpo da Página (Page Body)**

Onde reside a massa de dados, organizada logicamente para evitar sobrecarga cognitiva.

* **Seções (Sections):** O corpo é dividido em grandes blocos temáticos. A navegação entre seções pode ser feita via rolagem (scroll) ou através da *Anchor Bar* (barra de âncoras) no topo, que funciona como um menu de navegação rápida dentro da página.  
* **Subseções:** Dentro de cada seção, os dados podem ser ainda mais divididos.  
* **Conteúdo:** Pode conter formulários (FieldGroups), tabelas de itens filhos (LineItems), gráficos analíticos ou até componentes customizados.  
* **Configuração:** Definido via @UI.facet com purpose: #STANDARD (o padrão implícito).

### **3. O Conceito de Facetas (Facets)**

Para um iniciante em Fiori Elements, o termo "Faceta" pode parecer abstrato. Pense na Faceta como o **bloco de construção (LEGO)** do layout da Object Page. Sem facetas, a Object Page estaria vazia, mesmo que você tenha anotado todos os seus campos.

Uma **Faceta** é um elemento de metadados que diz ao Fiori Elements: "Pegue este grupo de campos e desenhe-os AQUI, com ESTE título".

Existem dois tipos principais de Facetas que você usará nas anotações, e entender a diferença é vital para criar layouts organizados:

#### **1. Reference Facet (Faceta de Referência)**

Esta é a faceta de "conteúdo". Ela não contém dados diretamente, mas **aponta** (referencia) para uma lista de anotações onde os dados estão definidos.

* **Função:** Renderizar o conteúdo real na tela.  
* **Tipos de Referência Comuns:**  
  * #IDENTIFICATION_REFERENCE: Aponta para campos anotados com @UI.identification. Desenha um formulário simples.  
  * #FIELDGROUP_REFERENCE: Aponta para um grupo específico de campos (@UI.fieldGroup). Permite ter múltiplos formulários na mesma tela (ex: "Dados de Endereço", "Dados Financeiros").  
  * #LINEITEM_REFERENCE: Aponta para uma associação (relação pai-filho). Desenha uma tabela completa com os itens da entidade filha.  
* **Visualmente:** Renderiza um formulário (Form Container) ou uma tabela (Smart Table).

#### **2. Collection Facet (Faceta de Coleção)**

Esta é a faceta de "estrutura". Ela é um container vazio projetado para agrupar outras facetas.

* **Função:** Agrupar múltiplas *Reference Facets* sob um único teto lógico ou visual.  
* **Nível Superior:** Quando usada no nível mais alto, uma Collection Facet cria uma nova **Seção (Aba)** na barra de navegação da Object Page.  
* **Visualmente:** Cria a estrutura da Seção. Dentro dela, você coloca as Reference Facets que aparecerão lado a lado ou uma abaixo da outra.

**Estrutura Hierárquica Típica:**

A construção de uma página profissional geralmente segue uma hierarquia de 2 ou 3 níveis:

* **Nível 1 (Collection Facet):** Cria a aba principal, por exemplo, "Informações Gerais".  
  * **Nível 2 (Reference Facet):** Cria o primeiro bloco de formulário dentro da aba, ex: "Dados Básicos" (Aponta para @UI.identification).  
  * **Nível 2 (Reference Facet):** Cria o segundo bloco ao lado ou abaixo, ex: "Dados Financeiros" (Aponta para @UI.fieldGroup: #Finance).

### **4. Diagrama da Estrutura (Object Page)**

Visualizar a hierarquia facilita a codificação da Metadata Extension.

[ Header Title: Viagem 1023 (Object Key) | Status: Aprovado (Status/Action) ]  
-----------------------------------------------------------------------------  
[ Header Content: Cliente: SAP | Preço: $500 (DataPoints / MicroCharts)     ]  
-----------------------------------------------------------------------------  
[ ⚓ Bar: Geral | Itens | Anexos ]  <-- Anchor Navigation Bar  
-----------------------------------------------------------------------------  
|  
|-- [ Seção 1: Geral ] (Collection Facet de Nível 1)  
|      |  
|      |-- [ Subseção A: Dados Básicos ] (Reference Facet -> @UI.identification)  
|      |      [ Campo ID   ]  
|      |      [ Campo Data ]  
|      |  
|      |-- [ Subseção B: Dados Financeiros ] (Reference Facet -> @UI.fieldGroup #Fin)  
|             [ Campo Preço ]  
|             [ Campo Moeda ]  
|  
|-- [ Seção 2: Itens ] (Reference Facet direta de Nível 1 -> @UI.lineItem da Associação)  
|      |  
|      |-- [ Tabela de Voos ]  
|             [ Coluna Voo ] [ Coluna Preço ]  
|

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Facet (Faceta):** Um elemento de anotação (@UI.facet) que define uma área específica de conteúdo na *Object Page*. É a unidade básica de layout, podendo representar uma seção inteira, um formulário ou uma tabela.  
* **Reference Facet:** Tipo de faceta que referencia diretamente uma anotação de UI (como identification, fieldGroup ou lineItem) para renderizar seu conteúdo. É o "tijolo" visível da construção.  
* **Collection Facet:** Tipo de faceta que atua como um container abstrato para agrupar múltiplas *Reference Facets*. Quando usada no nível raiz, representa uma Seção (Aba) navegável na página.  
* **Field Group:** Uma anotação (@UI.fieldGroup) usada para agrupar campos semanticamente relacionados sob um qualificador único. É referenciada por uma *Reference Facet* para exibir um bloco de formulário específico, permitindo múltiplos formulários na mesma seção.  
* **Header Facet:** Facetas posicionadas na área de cabeçalho colapsável da página (propriedade purpose: #HEADER). São usadas para exibir informações de alta prioridade, KPIs ou status que devem estar acessíveis sem rolagem.  
* **Anchor Bar:** A barra de navegação interna da Object Page que lista todas as Seções (Collection Facets de nível 1). Clicar em um item da barra rola a página automaticamente até a seção correspondente.

#### **Mapeamento: Faceta -> Visual**

| Tipo de Faceta | Propriedade Chave | Resultado Visual na Tela |
| :---- | :---- | :---- |
| **Header Facet** | purpose: #HEADER | Informação de destaque no topo da página (ex: Gráfico de Status), acima das abas. |
| **Collection Facet** | type: #COLLECTION | Uma nova aba ou seção de navegação na Anchor Bar da Object Page. |
| **Reference Facet** | type: #..._REFERENCE | Um bloco de formulário (com título próprio) ou uma tabela dentro de uma seção. |

### **📝 Quiz de Fixação**

Q1: Qual é a diferença funcional e estrutural entre uma Collection Facet e uma Reference Facet?  
R: A Collection Facet é estrutural; ela funciona como uma pasta ou container para organizar o layout (geralmente criando Seções/Abas). A Reference Facet é conteudista; ela aponta para as definições de campos ou tabelas (anotações de UI) que serão efetivamente renderizados dentro dessas seções.  
Q2: Se eu quiser adicionar uma lista de "Passageiros" (uma entidade filha) dentro da página de detalhes da "Viagem", que tipo de estrutura de faceta devo criar?  
R: Você deve criar uma Reference Facet do tipo #LINEITEM_REFERENCE. Esta faceta deve apontar para a associação de Passageiros (propriedade targetElement) e referenciará automaticamente a anotação @UI.lineItem definida na entidade Passageiro para desenhar a tabela.  
Q3: Onde, no processo de desenvolvimento RAP, são configuradas as Facetas e qual anotação é utilizada?  
R: As facetas são configuradas na camada de projeção, normalmente dentro das Metadata Extensions (.ddlx) associadas à Consumption View. A anotação utilizada é a @UI.facet, que aceita uma lista (array) de definições de facetas.