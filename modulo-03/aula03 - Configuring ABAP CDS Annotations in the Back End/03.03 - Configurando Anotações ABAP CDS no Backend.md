# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 03: Configurando Anotações ABAP CDS no Backend**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Mapear mentalmente como as anotações @UI no backend se traduzem em componentes visuais no frontend (Tabelas, Formulários, Cabeçalhos).  
2. Configurar o cabeçalho da aplicação usando **@UI.headerInfo**, definindo títulos dinâmicos e descrições de objeto.  
3. Definir e ordenar as colunas da tabela de resultados com **@UI.lineItem**, aplicando conceitos de importância (importance) para garantir a responsividade em dispositivos móveis.  
4. Criar filtros de pesquisa eficazes com **@UI.selectionField**, promovendo campos-chave para a barra de filtros padrão.  
5. Organizar campos na seção de detalhes usando **@UI.identification**, estruturando o formulário principal da Object Page.

### **1\. A Regra de Ouro: Anotações Guiam a UI**

No desenvolvimento com SAP Fiori Elements, abandonamos a ideia de "desenhar pixels" (arrastar botões, definir largura de div em CSS). Em vez disso, adotamos um paradigma declarativo: declaramos **intenções** via anotações @UI.

O framework Fiori Elements atua como um motor de renderização: ele lê essas intenções e constrói o HTML/JavaScript correspondente em tempo de execução. Isso garante que a interface esteja sempre em conformidade com as diretrizes de design da SAP (Fiori Guidelines), sem esforço extra do desenvolvedor.

Nesta aula, focaremos nas 4 anotações essenciais que transformam um serviço OData vazio em um aplicativo CRUD funcional.

### **2\. Cabeçalho da Entidade (@UI.headerInfo)**

Esta anotação é aplicada no **cabeçalho da CDS View** (ou da Metadata Extension) e define a identidade do objeto de negócio. Ela responde a perguntas como: "Como se chama este objeto no singular?", "E no plural?", "Qual campo representa o título principal?".

@UI.headerInfo: {   
    typeName: 'Viagem',   
    typeNamePlural: 'Viagens',   
    title: { type: \#STANDARD, value: 'TravelID' },  
    description: { type: \#STANDARD, value: 'Description' },  
    imageUrl: 'Attachment' " Opcional: Mostra imagem no cabeçalho se houver  
}  
annotate view Z\_C\_TRAVEL with ...

#### **Impacto Visual na Tela:**

1. **List Report:** O texto definido em typeNamePlural aparece no topo da tabela de resultados, geralmente acompanhado da contagem de registros (ex: "Viagens (50)").  
2. **Object Page:** O campo definido em title.value (neste caso, TravelID) é exibido com fonte grande e destaque no topo da página de detalhes. O campo description.value aparece logo abaixo, com fonte menor e cor cinza, fornecendo contexto adicional.

### **3\. A Lista de Relatório (@UI.lineItem)**

A anotação @UI.lineItem é responsável por desenhar as colunas da tabela principal no List Report. Cada campo anotado vira uma coluna.

#### **Propriedades Críticas:**

* **position:** Define a ordem das colunas da esquerda para a direita (10, 20, 30...). Recomenda-se usar intervalos de 10 para facilitar inserções futuras.  
* **label:** Define o texto do cabeçalho da coluna. Se omitido, o sistema usa o rótulo definido no Elemento de Dados (SE11) do campo.  
* **importance:** Fundamental para a responsividade. Controla quais colunas permanecem visíveis quando a tela diminui (Tablet/Celular).  
  * \#HIGH: A coluna nunca é escondida (ex: ID, Status).  
  * \#MEDIUM: Escondida em celulares, visível em tablets.  
  * \#LOW: Visível apenas em desktops grandes.

@UI.lineItem: \[   
  { position: 10, label: 'ID da Viagem', importance: \#HIGH, width: '10rem' }   
\]  
TravelID;

### **4\. A Barra de Filtros (@UI.selectionField)**

A anotação @UI.selectionField promove um campo para a **Smart Filter Bar** (a área de pesquisa no topo do List Report).

* **position:** Controla a ordem dos filtros da esquerda para a direita.  
* **Comportamento:** Se você adicionar esta anotação, o campo aparece como um input de filtro visível por padrão. Se não adicionar, o campo ainda pode ser usado para filtrar, mas o usuário precisará clicar no botão "Adaptar Filtros" para adicioná-lo manualmente.

@UI.selectionField: \[ { position: 10 } \]  
TravelID;

### **5\. A Página de Objeto (@UI.identification)**

Enquanto o lineItem cuida da tabela, o @UI.identification cuida do formulário. Ele lista os campos que devem aparecer na seção principal de informações da **Object Page**.

* **Agrupamento Padrão:** Por padrão, todos os campos anotados com identification são agrupados em uma faceta chamada "General Information" (se configurada).  
* **Edição:** Se a entidade for editável, esses campos se tornam inputs (caixas de texto, date pickers) automaticamente.

@UI.identification: \[ { position: 10, label: 'Viagem ID' } \]  
TravelID;

### **6\. Exemplo Prático Completo (Metadata Extension)**

Abaixo, apresentamos um arquivo de Metadata Extension (.ddlx) completo para a entidade "Agência de Viagens" (Z\_C\_AGENCY). Observe como combinamos todas as anotações para criar uma experiência rica.

@Metadata.layer: \#CORE

/\* 1\. Configuração do Cabeçalho Geral \*/  
@UI.headerInfo: {   
    typeName: 'Agência',   
    typeNamePlural: 'Agências',   
    title: { type: \#STANDARD, value: 'Name' },      // Nome da Agência como destaque  
    description: { type: \#STANDARD, value: 'AgencyID' } // ID como subtítulo  
}

annotate view Z\_C\_AGENCY with  
{  
  /\* 2\. Faceta (Estrutura da Object Page) \*/  
  /\* Cria uma seção visual para agrupar os campos de identificação \*/  
  @UI.facet: \[ { id: 'GeneralInfo',   
                 purpose: \#STANDARD,  
                 type: \#IDENTIFICATION\_REFERENCE,   
                 label: 'Informações Gerais',   
                 position: 10 } \]

  /\* 3\. Campos \*/  
    
  /\* UUID Técnico: Deve estar na projeção para funcionar, mas escondido do usuário \*/  
  @UI.hidden: true  
  AgencyUUID;

  /\* ID da Agência: Filtro \#1, Coluna \#1, Campo \#1 no detalhe \*/  
  @UI: {   
    lineItem:       \[ { position: 10, importance: \#HIGH } \],   
    selectionField: \[ { position: 10 } \],  
    identification: \[ { position: 10 } \]   
  }  
  AgencyID;

  /\* Nome: Filtro \#2, Coluna \#2, Campo \#2 no detalhe \*/  
  @UI: {   
    lineItem:       \[ { position: 20, importance: \#HIGH } \],   
    selectionField: \[ { position: 20 } \],  
    identification: \[ { position: 20 } \]   
  }  
  Name;

  /\* Telefone: Apenas na lista e detalhe, sem filtro \*/  
  @UI: {   
    lineItem:       \[ { position: 30, importance: \#MEDIUM } \],  
    identification: \[ { position: 30 } \]   
  }  
  PhoneNumber;

  /\* Cidade: Filtro \#3, Coluna \#4 com Label manual \*/  
  @UI: {   
    lineItem:       \[ { position: 40, label: 'Cidade Sede' } \],   
    selectionField: \[ { position: 30 } \],  
    identification: \[ { position: 40 } \]   
  }  
  City;  
}

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **@UI.headerInfo:** Anotação de nível de entidade (Entity-Level) que define os metadados descritivos do objeto de negócio. Controla o título da aba do navegador, o título da tabela no List Report e o cabeçalho da Object Page.  
* **@UI.lineItem:** Anotação de nível de campo (Field-Level) que instrui o Fiori Elements a renderizar o campo como uma coluna na tabela do List Report. Suporta configurações de largura, cor (criticality) e ações (botões na linha).  
* **@UI.selectionField:** Anotação de nível de campo que promove o campo para a barra de filtros (Smart Filter Bar). Campos sem essa anotação são considerados "filtros ocultos" disponíveis apenas via diálogo de adaptação.  
* **@UI.identification:** Anotação de nível de campo usada para construir o formulário principal da Object Page. Geralmente vinculada a uma faceta do tipo \#IDENTIFICATION\_REFERENCE.  
* **@UI.hidden:** Anotação booleana (true/false) usada para ocultar campos técnicos (chaves UUID, timestamps de sistema) da interface do usuário, mantendo-os disponíveis para a lógica interna do aplicativo.  
* **Importance (Responsividade):** Propriedade da anotação lineItem (\#HIGH, \#MEDIUM, \#LOW) que dita a prioridade de exibição da coluna em telas de larguras variadas, garantindo que a tabela não quebre o layout em dispositivos móveis.

#### **Mapeamento Visual: Anotação \-\> Tela**

| Anotação | Componente UI | Localização Típica |
| :---- | :---- | :---- |
| typeNamePlural | Título da Tabela | Topo do List Report (ex: "Viagens") |
| title.value | Título do Objeto | Cabeçalho da Object Page (Fonte Grande) |
| lineItem | Coluna de Tabela | Corpo do List Report |
| selectionField | Campo de Input | Barra Superior (Filter Bar) |
| identification | Campo de Formulário | Primeira Seção da Object Page |

### **📝 Quiz de Fixação**

Q1: Para que serve a propriedade importance: \#HIGH dentro da anotação @UI.lineItem e por que ela é importante para aplicativos móveis?  
R: Ela instrui o Fiori Elements a tratar aquela coluna como crítica, garantindo que ela permaneça visível mesmo quando o espaço da tela é reduzido (como em smartphones). Colunas com importância \#LOW ou \#MEDIUM são automaticamente ocultadas ou movidas para uma área de detalhes ("Pop-in") para evitar que a tabela gere uma barra de rolagem horizontal excessiva.  
Q2: Eu adicionei um campo na minha CDS View, mas ele não aparece na barra de filtros do aplicativo quando carrego a página. Qual anotação está faltando e como o usuário ainda poderia filtrar por esse campo?  
R: Está faltando a anotação @UI.selectionField. Sem ela, o campo não é promovido automaticamente para a barra visível. No entanto, o usuário ainda pode filtrar por ele clicando no botão "Adaptar Filtros" (Adapt Filters) na UI e selecionando o campo manualmente na lista de campos disponíveis.  
Q3: Qual a diferença fundamental de propósito entre as anotações @UI.headerInfo e @UI.identification?  
R: @UI.headerInfo é uma anotação de "identidade" global do objeto; ela configura os textos de título e descrição que aparecem no topo da página e na navegação. @UI.identification é uma anotação de "conteúdo"; ela define uma lista específica de campos que devem ser renderizados como um formulário dentro de uma seção da página de detalhes (Object Page).