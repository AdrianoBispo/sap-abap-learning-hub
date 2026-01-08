# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 08: Visão Geral do Conceito de Navegação em Apps SAP Fiori Elements**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar com precisão a **Navegação Interna** (Drill-down), que aprofunda o contexto dentro do mesmo objeto, da **Navegação Externa** (Cross-App), que transfere o contexto para um domínio de negócio diferente.  
2. Compreender a arquitetura de **Navegação Baseada em Intenção** (Intent-Based Navigation), explicando como o Fiori Launchpad atua como um "broker" para resolver links dinamicamente.  
3. Identificar e aplicar os componentes de uma Intenção: **Semantic Object** (O que é) e **Action** (O que fazer), construindo hashtags de navegação válidas.  
4. Configurar links de navegação inteligentes em tabelas usando a anotação \#FOR\_INTENT\_BASED\_NAVIGATION, garantindo a passagem correta de parâmetros de contexto.

### **1\. Tipos de Navegação: O Fluxo do Usuário**

No universo SAP Fiori, a navegação não é apenas "clicar num link para mudar de página". É um conceito projetado para suportar o fluxo de pensamento do usuário. Existem dois mecanismos distintos que servem a propósitos diferentes:

#### **A. Navegação Interna (Drill-down)**

Este é o padrão hierárquico. O usuário começa em uma visão ampla e mergulha nos detalhes.

* **Cenário:** Você está na lista de todas as viagens (List Report). Você clica na Viagem 100\. O sistema abre a página de detalhes dessa viagem (Object Page). Dentro dela, você clica na Reserva de Voo 50 e vê os detalhes do voo (Sub-Object Page).  
* **Configuração:** É praticamente **Automática**. Se você definiu as facetas e associações corretamente nas suas CDS Views e Metadata Extensions, o Fiori Elements "sabe" que a entidade Travel tem uma relação com Booking e cria os links de navegação interna sozinho. O contexto (ID da viagem) é preservado na URL interna do app.

#### **B. Navegação Externa (Cross-App)**

Este é o poder do ecossistema integrado. É quando o usuário muda de "papel" ou de "domínio".

* **Cenário:** Você está no App de "Aprovar Viagens". Você vê que a viagem é para o Cliente "SAP". Você quer ver se esse cliente tem faturas em aberto. Você clica no ID do Cliente e é redirecionado para o **App de Cockpit Financeiro do Cliente** (um aplicativo totalmente diferente, talvez até desenvolvido por outro time).  
* **Configuração:** Requer configuração explícita de **Objeto Semântico**. Você precisa dizer ao sistema: "Este campo não é apenas um texto 'SAP'; ele representa a entidade de negócio 'Customer'".

### **2\. O Segredo: Intent-Based Navigation (Navegação Baseada em Intenção)**

O SAP Fiori Launchpad resolve um problema antigo da web: links quebrados. Ele não usa URLs fixas e duras (como www.sap.com/app/vendas.html). Em vez disso, ele usa um sistema abstrato chamado **Intenções**.

Uma intenção é formada por duas partes principais, separadas por um hífen, precedidas por uma hashtag:  
\#ObjetoSemântico-Ação  
A Filosofia:  
O aplicativo de origem não diz "Abra o aplicativo Z\_APP\_CLIENTE". Ele diz "Eu tenho a intenção de Exibir um Cliente". O Launchpad intercepta esse pedido e decide qual aplicativo abrir.  
**Exemplos de Intenções Reais:**

* \#SalesOrder-display: O usuário quer ver detalhes de um pedido. (Pode abrir o App Fiori padrão de Vendas).  
* \#Customer-manage: O usuário quer editar/gerenciar um cliente. (Pode abrir o App de Dados Mestres).  
* \#Travel-analyze: O usuário quer ver gráficos. (Pode abrir um App Analítico ou Dashboard).

Como funciona (Target Mapping):  
No Launchpad Designer (ou Content Manager), o administrador cria um Target Mapping (Mapeamento de Destino).

* *Regra:* Se alguém chamar \#Customer-display...  
* *Ação:* ... abra o aplicativo BSP z\_customer\_cockpit.

Isso desacopla os apps. Se amanhã você trocar o app de clientes antigo por um novo, você só muda o Target Mapping. Todos os links em todos os apps do sistema continuam funcionando e agora apontam para o novo app automaticamente.

### **3\. Configurando Navegação na Tabela**

Para transformar uma coluna simples (ex: CustomerID) em um link clicável (Smart Link) que leva a outro aplicativo, usamos o tipo de linha \#FOR\_INTENT\_BASED\_NAVIGATION na anotação @UI.lineItem.

**Sintaxe na Metadata Extension:**

@UI.lineItem: \[   
  {   
    position: 30,   
    label: 'Cliente',  
      
    /\* Define que esta coluna é um link de navegação \*/  
    type: \#FOR\_INTENT\_BASED\_NAVIGATION,   
      
    /\* O "Substantivo": Sobre qual objeto de negócio estamos falando? \*/  
    semanticObject: 'Customer',      
      
    /\* O "Verbo": O que queremos fazer com ele? (display, edit, manage) \*/  
    semanticObjectAction: 'display'   
  }   
\]  
CustomerID;

**O Fluxo de Execução:**

1. **Renderização:** O Fiori Elements detecta a anotação e desenha o valor do CustomerID como um hiperlink azul.  
2. **Clique:** Quando o usuário clica, o framework dispara a intenção \#Customer-display.  
3. **Contexto:** O framework pega automaticamente o valor da linha clicada (ex: CustomerID='0001') e o anexa à intenção.  
4. **Resolução:** O Launchpad recebe \#Customer-display?CustomerID=0001 e abre o aplicativo de destino, já filtrado para o cliente 0001\.

### **4\. Navegação com Mapeamento de Parâmetros**

Um problema comum em integração é a divergência de nomes. Imagine que na sua tabela de Viagens, o campo se chama MyCustID, mas o aplicativo de destino (Cockpit do Cliente) espera receber um parâmetro chamado PartnerID. Se você apenas disparar a intenção, o destino não entenderá o parâmetro e abrirá a tela inicial sem filtro.

O Fiori Elements faz o mapeamento automático se os nomes forem iguais. Se forem diferentes, usamos anotações de **Parâmetros de Consumo** (@Consumption.semanticObject) para ensinar ao sistema o significado do campo.

**Exemplo Completo com Mapeamento:**

/\* Na CDS View ou Metadata Extension:  
   A anotação @Consumption.semanticObject diz: "Ei, sistema, embora este campo   
   se chame 'MyCustID', semanticamente ele equivale ao Objeto Semântico 'Customer'".  
   Isso ajuda o Smart Link a encontrar os parâmetros corretos.  
\*/  
@Consumption.semanticObject: 'Customer'   
MyCustID;

/\* Na Metadata Extension (Definição da Coluna) \*/  
annotate view Z\_C\_TRAVEL with {  
    @UI.lineItem: \[   
        {   
          type: \#FOR\_INTENT\_BASED\_NAVIGATION,   
          semanticObject: 'Customer',   
          semanticObjectAction: 'display',   
          label: 'Ver Cliente'   
        }   
    \]  
    MyCustID;  
}

*Nota:* Em cenários complexos onde a anotação @Consumption não é suficiente, pode ser necessário configurar o mapeamento de parâmetros manualmente no Target Mapping do Launchpad.

### **5\. Navegação para URL (Links da Web)**

Às vezes, o destino não é um aplicativo SAP Fiori, mas uma página externa (Google, LinkedIn, Sistema Legado Web, Rastreamento de Correios). Para isso, usamos o tipo \#WITH\_URL.

Diferente da navegação baseada em intenção, aqui o link é "duro" (Hardcoded) ou vem de um campo de dados.

@UI.lineItem: \[   
  {   
    type: \#WITH\_URL,   
    /\* O valor da URL vem do conteúdo deste campo na view \*/  
    url: 'WebAddress',  
    label: 'Website'  
  }   
\]  
WebsiteLink;

**Cuidados:**

* Links externos abrem em novas abas por padrão.  
* Não use \#WITH\_URL para navegar para outros apps SAP internos; você perderá a flexibilidade do Launchpad e a gestão de sessão.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Intent-Based Navigation (Navegação Baseada em Intenção):** Mecanismo central de navegação do SAP Fiori Launchpad que desacopla a aplicação de origem da aplicação de destino. Em vez de links diretos, usa-se uma intenção semântica (\#Object-Action), permitindo que o sistema resolva dinamicamente qual app abrir baseando-se nas permissões e configurações do usuário.  
* **Semantic Object (Objeto Semântico):** Representa uma entidade de negócio abstrata (ex: Customer, Product, SalesOrder, CostCenter). É a primeira parte da hashtag de navegação e serve para agrupar todas as aplicações relacionadas a esse conceito.  
* **Semantic Action (Ação Semântica):** Representa a intenção do que se deseja fazer com o objeto (ex: display, edit, manage, approve, analyze). Diferentes ações no mesmo objeto podem levar a aplicativos completamente diferentes.  
* **Target Mapping (Mapeamento de Destino):** Configuração administrativa feita no Launchpad Designer que conecta uma Intenção (\#Sales-display) a um aplicativo técnico real (Componente SAPUI5 ou Transação GUI). É a "tabela de roteamento" do Fiori.  
* **Drill-down:** Navegação vertical/hierárquica dentro do mesmo aplicativo, movendo-se de uma lista geral para uma visão detalhada (List Report \-\> Object Page), preservando o contexto da sessão.

#### **Fluxo da Navegação Externa (Passo a Passo)**

1. **Usuário:** Clica no link "Cliente 100" na tabela de Viagens.  
2. **App de Origem:** Dispara a intenção \#Customer-display e anexa o contexto ?CustomerID=100.  
3. **Fiori Launchpad:** Intercepta a intenção. Verifica nos catálogos do usuário qual Target Mapping corresponde a \#Customer-display.  
4. **Resolução:** Encontra que essa intenção aponta para o App "Z\_CUSTOMER\_360".  
5. **App de Destino:** O Launchpad carrega o App "Z\_CUSTOMER\_360" e repassa o parâmetro CustomerID=100. O app inicia já filtrando esse cliente.

### **📝 Quiz de Fixação**

Q1: Qual a diferença fundamental de uso entre a anotação \#FOR\_INTENT\_BASED\_NAVIGATION e \#WITH\_URL?  
R: \#FOR\_INTENT\_BASED\_NAVIGATION é usada para navegar para outros aplicativos Fiori dentro do ecossistema SAP/Launchpad usando abstrações semânticas (Objeto-Ação), garantindo integração e flexibilidade. \#WITH\_URL é usada para abrir links externos arbitrários da internet (como Google ou portais de parceiros) baseados em um endereço web absoluto.  
Q2: Se eu quiser que um clique no campo SalesOrderID abra o aplicativo padrão de exibição de pedidos de venda, quais informações obrigatórias preciso fornecer na anotação da linha?  
R: Preciso fornecer o type: \#FOR\_INTENT\_BASED\_NAVIGATION, o semanticObject (ex: SalesOrder) e a semanticObjectAction (ex: display ou manage).  
Q3: Como o aplicativo de destino sabe exatamente qual registro abrir quando o usuário clica em um link de navegação?  
R: O Fiori Elements (framework de origem) passa automaticamente os valores da linha selecionada (o contexto, como o ID) como parâmetros de URL na chamada da intenção. O aplicativo de destino é projetado para ler esses parâmetros de URL na sua inicialização (Startup Parameters) e usá-los para filtrar os dados ou carregar o registro específico.