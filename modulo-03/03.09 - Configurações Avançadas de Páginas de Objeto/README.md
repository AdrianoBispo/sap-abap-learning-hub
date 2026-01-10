# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 09: Configurações Avançadas de Páginas de Objeto**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Configurar **Header Facets** para exibir informações críticas (KPIs, Status, Contatos) no cabeçalho colapsável da página, melhorando a capacidade de decisão rápida do usuário.  
2. Aplicar **Visibilidade Dinâmica** (@UI.hidden) para mostrar ou ocultar campos, botões e facetas inteiras baseados em condições lógicas do backend, limpando a interface de elementos irrelevantes.  
3. Entender e implementar o uso de **Sub-Seções** aninhadas para organizar formulários longos em blocos lógicos menores, evitando a fadiga de rolagem.  
4. Identificar como referenciar Associações (targetElement) para criar abas que contêm **Tabelas Filhas** (Listas de Itens), permitindo a gestão de relacionamentos 1:N (Um para Muitos).

### **1. Header Facets: Destacando o Importante**

A **Object Page** não se resume apenas ao corpo do formulário com abas. Uma das áreas mais valiosas e subutilizadas da tela é o **Header Content** (Conteúdo do Cabeçalho), a área retangular expansível localizada logo abaixo do título principal.

Por que usar Header Facets?  
Em cenários de negócio complexos, o usuário muitas vezes precisa tomar uma decisão rápida (ex: "Aprovo ou Rejeito este orçamento?") sem ter que rolar por dezenas de campos de formulário. O Header Content serve como um Dashboard Resumido do objeto em foco. É o imóvel mais nobre da tela, reservado para KPIs (Key Performance Indicators), métricas críticas e status de alto nível.  
Tipos de Conteúdo no Cabeçalho:  
Embora possamos colocar grupos de campos simples, o cabeçalho brilha quando utilizamos visualizações semânticas ricas:

* **Data Points:** Valores únicos exibidos com fonte grande e formatação de destaque (ex: Cores de criticidade, setas de tendência "Up/Down").  
* **Micro Charts:** Pequenos gráficos (Bullet, Radial, Progress) que mostram a saúde do registro visualmente (ex: "% do Orçamento Utilizado").  
* **Contact Details:** Cartões de visita com foto, nome e links diretos para e-mail/telefone (usando a anotação @Communication.contact).

Configuração Técnica:  
Para posicionar um elemento nesta área, utilizamos a mesma anotação @UI.facet que usamos para o corpo da página, mas mudamos a propriedade purpose para #HEADER. O framework automaticamente entende que este bloco não pertence às abas de navegação (Anchor Bar), mas sim ao painel superior.  
@UI.facet: [  
  /* Faceta de Cabeçalho: Mostra o Preço no topo */  
  {  
    id: 'HeaderPrice',  
    purpose: #HEADER,            /* A Mágica acontece aqui: Move para o topo */  
    type: #DATAPOINT_REFERENCE,  /* Indica que vai renderizar um valor numérico formatado */  
    targetQualifier: 'PriceData', /* Aponta para a anotação @UI.dataPoint no campo */  
    position: 10  
  },  
    
  /* Faceta Padrão (Corpo da página) */  
  /* Se 'purpose' for omitido, assume-se #STANDARD */  
  {  
    id: 'General',  
    purpose: #STANDARD,   
    type: #COLLECTION,  
    label: 'Informações Gerais',  
    position: 10  
  }  
]

*Nota:* O uso de #DATAPOINT_REFERENCE no cabeçalho é a melhor prática para exibir valores monetários, pontuações ou contadores, pois permite aplicar cores semânticas (Verde/Vermelho) independentes da tabela.

### **2. Visibilidade Dinâmica (Hidden)**

Um dos princípios do Fiori é a **Simplicidade**. Mostrar campos que não são relevantes para o contexto atual apenas confunde o usuário. Um requisito funcional comum é: *"O campo 'Motivo da Rejeição' só deve aparecer se o Status da viagem for 'Rejeitado'. Caso contrário, ele deve sumir."*

No Fiori Elements, não escrevemos JavaScript (if status == 'X' show()) para isso. Controlamos a visibilidade declarativamente apontando a propriedade hidden para um campo booleano calculado na CDS View.

#### **Passo 1: Lógica no Backend (CDS View)**

Criamos um campo técnico calculado (geralmente não exibido na tela) que retorna true quando o elemento alvo deve ser escondido.

/* Na CDS View Z_I_TRAVEL */  
/* Lógica: Esconder se NÃO for Rejeitado ('X') */  
case when overall_status = 'X' then '' else 'X' end as IsRejectionHidden,

/* Lógica: Esconder botões de edição se já estiver aprovado */  
case when overall_status = 'A' then 'X' else '' end as IsReadOnly

*Dica:* No ABAP CDS, um campo de caracteres vazio '' é tratado como false (não esconder), e um campo com valor 'X' é tratado como true (esconder).

#### **Passo 2: Anotação no Frontend (Metadata Extension)**

Na Metadata Extension, vinculamos a propriedade hidden ao campo calculado usando a sintaxe #(NomeDoCampo). Isso cria um "binding" dinâmico.

/* No campo Motivo da Rejeição */  
@UI: {   
  identification: [ { position: 100 } ],  
  /* Ocultação Dinâmica: O framework lê 'IsRejectionHidden' a cada alteração de dados */  
  hidden: #(IsRejectionHidden)   
}  
RejectionReason;

**Poder da Visibilidade:**

* **Granularidade:** Isso funciona para **campos individuais**, para **ações** (botões) e até para **Facetas inteiras** (esconder uma aba inteira se não for relevante).  
* **Reatividade:** Se o usuário mudar o status na tela de "Aberto" para "Rejeitado", o campo "Motivo" aparecerá instantaneamente sem precisar recarregar a página (Side Effects).

### **3. Tabelas Filhas (Line Item Reference)**

Uma Object Page raramente mostra apenas dados do cabeçalho ("Dados do Pai"). A riqueza de um sistema ERP está nos relacionamentos. Uma Viagem tem **Reservas**; um Pedido tem **Itens**.

Para criar uma aba (Seção) que contém uma tabela de itens relacionados, criamos uma Faceta especial do tipo **#LINEITEM_REFERENCE**.

Como funciona a conexão?  
Diferente das facetas de formulário que olham para campos da própria view, a faceta de tabela olha para uma Navegação. Você deve fornecer o nome da Associação exposta na CDS View.  
{  
  id: 'BookingTab',  
  type: #LINEITEM_REFERENCE, /* Define que o conteúdo é uma Tabela */  
  label: 'Reservas de Voo',   /* Título da Aba */  
  position: 20,  
  targetElement: '_Booking'  /* Nome Exato da Associação na CDS View */  
}

**Pontos Críticos de Design:**

1. **Colunas da Tabela Filha:** As colunas que aparecerão nesta tabela **não** são definidas na Metadata Extension da Viagem (Pai). Elas são definidas pelas anotações @UI.lineItem na Metadata Extension da entidade **Booking** (Filho). O Fiori Elements carrega os metadados do filho automaticamente.  
2. **CRUD de Itens:** Se o Behavior Definition permitir (create, delete na entidade filho), a tabela renderizada automaticamente ganhará botões de "Criar" e "Excluir", permitindo gerenciar os itens diretamente da tela do pai.

### **4. Exemplo Prático Completo**

Vamos configurar uma Object Page de Viagem profissional que utiliza todos os conceitos: Cabeçalho com KPI, Corpo com Seções aninhadas e Tabela de Itens.

**Cenário:**

1. **Cabeçalho:** Exibir o "Custo Total" em destaque.  
2. **Aba "Geral":**  
   * **Bloco 1:** Dados de Identificação.  
   * **Bloco 2 (Dinâmico):** Motivo da Rejeição (só se rejeitado).  
3. **Aba "Reservas":** Lista completa dos voos associados.

@Metadata.layer: #CORE  
@UI.headerInfo: { typeName: 'Viagem', title: { value: 'TravelID' } }

annotate view Z_C_TRAVEL with  
{  
  /* --- ESTRUTURA DE FACETAS --- */  
  @UI.facet: [  
    /* 1. HEADER FACET: KPI de Preço */  
    /* Aparece na área cinza superior, expansível */  
    {  
      id: 'HeaderPrice',  
      purpose: #HEADER,  
      type: #DATAPOINT_REFERENCE,  
      targetQualifier: 'PricePoint', /* Link para anotação @UI.dataPoint abaixo */  
      position: 10  
    },

    /* 2. COLLECTION FACET: Aba "Geral" */  
    /* Atua como container para os formulários */  
    {  
      id: 'General',  
      type: #COLLECTION,  
      label: 'Visão Geral',  
      position: 10  
    },  
      /* 2.1 REFERENCE FACET: Sub-seção de Dados Básicos */  
      /* Renderizada DENTRO da aba Geral (via parentId) */  
      {  
        id: 'TravelData',  
        parentId: 'General',  
        type: #IDENTIFICATION_REFERENCE,  
        label: 'Dados da Viagem',  
        position: 10  
      },

    /* 3. REFERENCE FACET: Aba "Reservas" (Tabela Filha) */  
    /* Cria uma nova aba dedicada para a lista de itens */  
    {  
      id: 'Bookings',  
      type: #LINEITEM_REFERENCE,  
      label: 'Reservas de Voo',  
      position: 20,  
      targetElement: '_Booking' /* Navegação para a entidade filha */  
    }  
  ]

  /* --- CONFIGURAÇÃO DOS CAMPOS --- */  
    
  /* Configuração do DataPoint para o Header */  
  /* title: Rótulo pequeno acima do número */  
  @UI.dataPoint: { qualifier: 'PricePoint', title: 'Custo Total', criticality: 'StatusCriticality' }  
  TotalPrice;

  /* Campos do Formulário Principal */  
  @UI.identification: [{ position: 10 }]  
  TravelID;

  @UI.identification: [{ position: 20 }]  
  Description;  
    
  /* Campo com Visibilidade Dinâmica */  
  @UI.identification: [{ position: 30 }]  
  @UI.hidden: #(IsRejectionHidden)   
  RejectionReason;  
}

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Header Facet (purpose: #HEADER):** Faceta especial posicionada na área de cabeçalho colapsável da Object Page. É desenhada separadamente do corpo da página e é usada para exibir KPIs, DataPoints ou informações de contato que devem estar sempre acessíveis, independentemente da aba selecionada.  
* **Dynamic Visibility (Hidden):** Capacidade de ocultar ou exibir elementos da UI (campos, botões, grupos) em tempo de execução, baseando-se no valor de outro campo. Isso permite interfaces adaptativas que reagem ao estado do dado (ex: Rascunho vs. Ativo, Aprovado vs. Rejeitado).  
* **#DATAPOINT_REFERENCE:** Tipo de faceta usada especificamente para exibir um único valor numérico ou textual com grande destaque visual (fonte ampliada). É ideal para cabeçalhos e requer a anotação complementar @UI.dataPoint no campo correspondente para definir título e cor.  
* **#LINEITEM_REFERENCE:** Tipo de faceta usada para renderizar uma tabela de itens relacionados (relação 1:N). Exige a propriedade targetElement, que deve apontar para o nome da Associação CDS. As colunas dessa tabela são definidas na Metadata Extension da entidade associada.  
* **Target Element:** Propriedade mandatória em facetas do tipo #LINEITEM_REFERENCE e #CHART_REFERENCE. Indica qual associação (caminho de navegação) o Fiori Elements deve seguir para buscar os dados que povoarão aquele componente visual.

#### **Guia de Aninhamento de Facetas**

1. **Header (purpose: #HEADER):**  
   * Visível sempre (a menos que colapsado pelo usuário).  
   * Não aceita aninhamento profundo (não se usa Collection Facets aqui).  
   * Ideal para: DataPoints, MicroCharts, ContactCards.  
2. **Section (Nível Raiz do Corpo):**  
   * É uma Collection Facet com purpose: #STANDARD (implícito).  
   * Cria um item clicável na **Anchor Bar** (Barra de Navegação).  
3. **Sub-Section (Bloco Interno):**  
   * É uma Reference Facet (#IDENTIFICATION, #FIELDGROUP ou #LINEITEM).  
   * Possui a propriedade parentId apontando para o ID da Section.  
   * Renderiza o conteúdo real (Formulário ou Tabela).

### **📝 Quiz de Fixação**

Q1: Qual a diferença visual e funcional entre definir uma faceta com purpose: #STANDARD e purpose: #HEADER?  
R: #STANDARD (o padrão) coloca a faceta no corpo principal da página, organizado em abas ou seções de rolagem. #HEADER coloca a faceta na área de cabeçalho superior, acima da barra de navegação. Funcionalmente, o conteúdo do #HEADER serve como um resumo de alto nível e KPI, enquanto o #STANDARD contém o detalhamento operacional e formulários de edição.  
Q2: Para exibir uma lista de itens (tabela filha) dentro da página de detalhes do pai, que tipo de faceta devo usar e qual propriedade é obrigatória para conectar os dados?  
R: Deve-se usar o tipo #LINEITEM_REFERENCE. A propriedade obrigatória é targetElement, que deve conter o nome exato da Associação (definida na CDS View) que aponta para a entidade filha (ex: _Booking). O framework usa essa associação para fazer a query dos itens.  
Q3: Como posso ocultar dinamicamente um campo "Comentário" se a viagem estiver com status "Em Aberto", sem usar JavaScript?  
R: Primeiro, crie um campo calculado na CDS View (ex: IsCommentHidden) que retorna 'X' (true) se o status for "Em Aberto" e '' (false) caso contrário. Em seguida, na Metadata Extension, anote o campo "Comentário" com @UI.hidden: #(IsCommentHidden). O Fiori Elements fará o binding e controlará a visibilidade automaticamente.