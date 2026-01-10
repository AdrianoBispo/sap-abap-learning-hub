# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 06: Configurando Relatórios de Lista e Páginas de Objeto**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Aplicar **Ordenação Padrão** (Sorting) na tabela do List Report usando a anotação complexa PresentationVariant, compreendendo como isso afeta a percepção inicial do usuário.  
2. Implementar a estrutura hierárquica de **Facetas** na Metadata Extension para transformar uma Object Page vazia em uma interface organizada com Seções e Subseções.  
3. Utilizar **FieldGroups** estrategicamente para quebrar formulários longos em blocos lógicos menores (ex: Datas, Preços, Logística), melhorando a legibilidade.  
4. Diferenciar tecnicamente e funcionalmente o uso de #IDENTIFICATION_REFERENCE (Bloco Padrão) e #FIELDGROUP_REFERENCE (Blocos Customizados).

### **1. Refinando o List Report: Ordenação Padrão**

Quando um usuário abre um aplicativo, a primeira impressão é vital. Por padrão, o banco de dados retorna os registros na ordem da chave primária (ex: UUID) ou na ordem de inserção, o que raramente é útil para o negócio. O usuário geralmente quer ver "o que aconteceu por último" ou "o que é mais urgente".

Para controlar isso sem obrigar o usuário a clicar manualmente no cabeçalho da coluna, usamos a anotação de nível de entidade **@UI.presentationVariant**.

Esta anotação é poderosa porque ela não controla apenas a ordenação, mas também pode pré-definir agrupamentos e agregações iniciais.

@Metadata.layer: #CORE  
@UI.headerInfo: { ... }

/* Presentation Variant: Define como os dados são apresentados inicialmente.  
   - sortOrder: Lista de campos para ordenação.  
   - visualizations: Liga essa variante à visualização de tabela (LineItem).  
*/  
@UI.presentationVariant: [{   
    sortOrder: [  
      { by: 'BeginDate', direction: #DESC },  /* Critério 1: Data mais recente primeiro */  
      { by: 'OverallStatus', direction: #ASC } /* Critério 2: Agrupar status iguais */  
    ],   
    visualizations: [{ type: #AS_LINEITEM }]   
}]

annotate view Z_C_TRAVEL with ...

**Por que usar?**

* **Eficiência:** O usuário economiza cliques.  
* **Foco:** Você direciona a atenção do usuário para os itens mais recentes ou críticos.

### **2. Estruturando a Object Page: O Código das Facetas**

Na aula anterior, aprendemos a teoria de *Collection* e *Reference Facets*. Agora, vamos traduzir isso para o código da Metadata Extension (.ddlx).

A anotação **@UI.facet** é estruturada como uma matriz (array) de objetos. A ordem no array define a ordem na tela (de cima para baixo, ou da esquerda para a direita).

#### **A. Criando a Seção Principal (Collection Facet)**

O primeiro passo é criar o "Container". No Fiori Elements, uma *Collection Facet* de nível superior cria uma **Seção** (uma aba na barra de navegação da Object Page).

@UI.facet: [  
  /* Nível 1: A Seção (Collection)   
     Isso cria a aba "Informações Gerais" na barra de navegação.  
     Ela não mostra dados sozinha; ela segura outros blocos.  
  */  
  {  
    id: 'GeneralSection',   /* ID único técnico (obrigatório) */  
    type: #COLLECTION,      /* Define que é um container */  
    label: 'Informações Gerais',  
    position: 10            /* Ordem da aba na página */  
  },  
    
  /* Nível 2: Os Blocos dentro da Seção seriam definidos aqui abaixo */  
  ...  
]

### **3. Preenchendo a Seção: Identification vs. FieldGroup**

Uma vez que temos a seção "Geral", precisamos preenchê-la com formulários. Temos duas ferramentas principais para isso, e a escolha depende da complexidade dos seus dados.

#### **Estratégia A: #IDENTIFICATION_REFERENCE**

Esta é a abordagem "simples e direta". Existe um grupo especial de anotações chamado @UI.identification.

* **O que é:** Uma faceta deste tipo renderiza **todos** os campos que você marcou com @UI.identification na sua Metadata Extension.  
* **Limitação:** Você só tem **um** grupo de identificação por entidade. Você não pode dividir esses campos em dois blocos visuais separados (ex: lado a lado). Eles sempre aparecerão juntos em uma única lista vertical.  
* **Uso Ideal:** Para o bloco principal de "Cabeçalho" ou dados essenciais (ID, Descrição, Status).

/* Definição da Faceta */  
{  
  id: 'BasicData',  
  purpose: #STANDARD,  
  type: #IDENTIFICATION_REFERENCE, /* Aponta para @UI.identification */  
  label: 'Dados Básicos',  
  parentId: 'GeneralSection',      /* CRUCIAL: Aninha este bloco DENTRO da aba Geral */  
  position: 10  
}

#### **Estratégia B: #FIELDGROUP_REFERENCE**

Esta é a abordagem "flexível". Ela permite criar quantos grupos de campos você quiser.

* **O que é:** Funciona através de um sistema de etiquetas (tags). Você etiqueta um campo com um qualifier (ex: 'PriceData') e cria uma faceta que busca apenas campos com essa etiqueta.  
* **Vantagem:** Permite layout de múltiplas colunas. Você pode colocar o bloco "Preços" na posição 10 e o bloco "Datas" na posição 20 dentro da mesma seção pai, e o Fiori Elements tentará renderizá-los lado a lado se houver espaço na tela.

/* 1. Definição da Faceta na lista @UI.facet */  
{  
  id: 'Prices',  
  type: #FIELDGROUP_REFERENCE,  
  label: 'Valores Financeiros',  
  parentId: 'GeneralSection',      /* Também vai dentro da aba Geral */  
  position: 20,                    /* Aparece DEPOIS ou AO LADO dos Dados Básicos */  
  targetQualifier: 'PriceData'     /* O Elo de Ligação: Busca campos com esta tag */  
}

/* 2. Anotação nos Campos (Vinculando ao Grupo) */  
@UI.fieldGroup: [{ qualifier: 'PriceData', position: 10, label: 'Preço Líquido' }]  
TotalPrice;

@UI.fieldGroup: [{ qualifier: 'PriceData', position: 20, label: 'Taxa de Reserva' }]  
BookingFee;

### **4. Exemplo Prático Completo**

Vamos juntar tudo. Abaixo, uma Metadata Extension que cria uma Object Page sofisticada. Ela possui uma aba "Geral" que organiza as informações em duas colunas: "Dados da Viagem" (Esquerda) e "Planejamento" (Direita).

@Metadata.layer: #CORE  
@UI.headerInfo: { typeName: 'Viagem', title: { value: 'TravelID' } }

/* Ordenação padrão da lista */  
@UI.presentationVariant: [{ sortOrder: [{ by: 'BeginDate', direction: #DESC }] }]

annotate view Z_C_TRAVEL with  
{  
  /* --- ESTRUTURA VISUAL (FACETAS) --- */  
  @UI.facet: [  
    /* 1. Container Principal (Aba Geral) */  
    {  
      id: 'General',  
      type: #COLLECTION,  
      label: 'Visão Geral',  
      position: 10  
    },  
      
    /* 2. Bloco Esquerdo: Dados Principais */  
    /* Usa IDENTIFICATION para os dados core */  
    {  
      id: 'TravelData',  
      parentId: 'General', /* Filho de 'General' */  
      type: #IDENTIFICATION_REFERENCE,  
      label: 'Dados da Viagem',  
      position: 10         /* Posição 10 = Primeiro Bloco / Coluna Esquerda */  
    },

    /* 3. Bloco Direito: Datas e Prazos */  
    /* Usa FIELDGROUP para separar logicamente as datas */  
    {  
      id: 'DateData',  
      parentId: 'General', /* Filho de 'General' */  
      type: #FIELDGROUP_REFERENCE,  
      label: 'Planejamento e Datas',  
      position: 20,        /* Posição 20 = Segundo Bloco / Coluna Direita */  
      targetQualifier: 'DatesGroup' /* Busca campos marcados com 'DatesGroup' */  
    }  
  ]

  /* --- CONFIGURAÇÃO DOS CAMPOS --- */

  /* Campos do Bloco Esquerdo (Identification) */  
  @UI.identification: [{ position: 10 }]   
  TravelID;

  @UI.identification: [{ position: 20 }]   
  AgencyID;

  @UI.identification: [{ position: 30 }]   
  CustomerID;

  /* Campos do Bloco Direito (FieldGroup: DatesGroup) */  
  @UI.fieldGroup: [{ qualifier: 'DatesGroup', position: 10 }]   
  BeginDate;

  @UI.fieldGroup: [{ qualifier: 'DatesGroup', position: 20 }]  
  EndDate;  
    
  /* Dica: Campos sem anotação identification/fieldGroup não aparecem no detalhe,  
     mesmo que estejam na CDS View. */  
}

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **@UI.presentationVariant:** Anotação de nível de entidade usada para definir o estado inicial da tabela. Controla a ordenação (sortOrder), agrupamento (groupBy) e qual visualização usar (visualizations). Essencial para UX.  
* **#COLLECTION (Facet Type):** Define uma faceta que atua puramente como um container estrutural. Visualmente, transforma-se em uma Seção (Aba) na Object Page. É obrigatório ter um id único para que sub-facetas possam referenciá-lo via parentId.  
* **#IDENTIFICATION_REFERENCE:** Tipo de faceta que renderiza automaticamente todos os campos que possuem a anotação @UI.identification. É o bloco de campos "Padrão" da entidade, útil para dados de cabeçalho.  
* **#FIELDGROUP_REFERENCE:** Tipo de faceta mais flexível que renderiza campos agrupados por um qualifier específico (@UI.fieldGroup: { qualifier: '...' }). Permite criar múltiplos blocos de formulário distintos na mesma página.  
* **ParentId:** Propriedade da faceta que estabelece a hierarquia visual. Se uma faceta B tem parentId: 'A', ela será renderizada *dentro* da faceta A. Isso permite layouts aninhados (Blocos dentro de Seções).  
* **Qualifier:** Uma string arbitrária usada para conectar um grupo de campos a uma faceta específica. É a "cola" que liga a anotação do campo à estrutura da página.

#### **Guia de Decisão: Identification vs FieldGroup**

| Cenário | Use Identification (#IDENTIFICATION_REFERENCE) | Use FieldGroup (#FIELDGROUP_REFERENCE) |
| :---- | :---- | :---- |
| **Quantidade de Blocos** | Apenas 1 bloco principal. | Múltiplos blocos (2, 3, 10...). |
| **Layout** | Lista vertical única. | Permite layout de múltiplas colunas (lado a lado). |
| **Semântica** | Dados de identificação geral. | Dados temáticos (Financeiro, Logística, Auditoria). |
| **Complexidade** | Baixa / Rápida implementação. | Média / Exige definição de Qualifiers. |

### **📝 Quiz de Fixação**

Q1: Como faço para que a lista de viagens apareça ordenada pela data de início (decrescente) assim que o usuário abrir o aplicativo, sem que ele precise clicar na coluna?  
R: Adicionando a anotação @UI.presentationVariant no cabeçalho da Metadata Extension. Dentro dela, deve-se definir a propriedade sortOrder apontando para o campo BeginDate com a direção #DESC.  
Q2: Se eu quiser criar duas caixas de formulário separadas na mesma aba da Object Page (ex: uma caixa para "Dados do Cliente" e outra para "Dados de Contato"), qual tipo de referência de faceta devo usar e por quê?  
R: Devo usar #FIELDGROUP_REFERENCE. O tipo #IDENTIFICATION_REFERENCE só permite criar um único bloco de campos. Com #FIELDGROUP_REFERENCE, posso usar qualificadores diferentes (ex: qualifier: 'Cliente' e qualifier: 'Contato') para agrupar os campos em blocos distintos e criar duas facetas separadas apontando para cada qualificador.  
Q3: O que acontece visualmente se eu criar uma faceta com parentId apontando para um ID que não existe na minha lista de facetas?  
R: A configuração ficará órfã. Dependendo da versão do Fiori Elements, o bloco pode não aparecer na tela ou pode ser renderizado de forma "solta" no final da página, fora da estrutura de abas desejada. É crucial que o parentId corresponda exatamente ao id de uma faceta do tipo #COLLECTION.