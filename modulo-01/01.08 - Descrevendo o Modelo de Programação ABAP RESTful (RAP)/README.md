# **Módulo 01: Programação ABAP Básica**

## **Aula 08: Descrevendo o Modelo de Programação ABAP RESTful (RAP)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Explicar em profundidade a arquitetura de três camadas do modelo RAP e como cada camada contribui para o desacoplamento e a manutenção do software.  
2. Analisar criticamente os cenários de implementação **Managed** (Gerenciado) e **Unmanaged** (Não Gerenciado), decidindo qual utilizar com base nos requisitos de legado e complexidade do projeto.  
3. Dominar o fluxo de desenvolvimento ponta a ponta (End-to-End): da definição da Tabela de Banco de Dados até a pré-visualização da UI no navegador.  
4. Identificar e manipular os artefatos de projeção e exposição de serviços: **Service Definition** e **Service Binding**, compreendendo suas configurações de protocolo (OData V2/V4).

### **1. O que é o RAP? A Evolução do Desenvolvimento SAP**

O **ABAP RESTful Application Programming Model (RAP)** não é apenas uma "nova ferramenta"; é a culminação de décadas de evolução no desenvolvimento SAP. Ele substitui modelos anteriores fragmentados (como o modelo clássico de Dynpro, o Web Dynpro ABAP e o modelo de programação BOPF/Gateway híbrido) por uma arquitetura unificada e opinativa.

O RAP é a arquitetura padrão para construir aplicações **SAP S/4HANA** modernas, sejam elas On-Premise (a partir da versão 1909) ou na Nuvem (**ABAP Cloud**). Ele foi desenhado nativamente para o banco de dados SAP HANA, permitindo o desenvolvimento de serviços OData (APIs Web) prontos para empresas, otimizados e intrinsecamente seguros.

**Principais Benefícios:**

* **Padronização:** Define uma maneira única e clara de implementar operações transacionais (CUD - Create, Update, Delete).  
* **Agnosticismo de Protocolo:** Embora focado em OData, a lógica de negócio é desacoplada do protocolo HTTP, facilitando testes e reutilização.  
* **Fiori Nativo:** As aplicações RAP nascem com suporte nativo a anotações de UI que alimentam o SAP Fiori Elements.

### **2. A Arquitetura em Três Camadas**

O RAP é estruturado como um "bolo" de três camadas principais, onde cada camada tem uma responsabilidade estrita. O desenvolvimento sempre acontece de baixo para cima (Bottom-Up), garantindo que a base de dados suporte a lógica, que por sua vez suporta a interface.

#### **Camada 1: Data Modeling & Behavior (O Coração)**

Esta é a camada onde reside a "verdade" do negócio.

* **CDS Views (Core Data Services):** Definem o modelo de dados semântico (o "O Que"). Aqui construímos o **Virtual Data Model (VDM)**. Diferente das views SQL clássicas, as CDS Views contêm associações ricas, cálculos de campos e metadados de domínio.  
* **Behavior Definition (BDEF):** Define o comportamento transacional (o "Como"). É um artefato exclusivo do RAP onde declaramos explicitamente:  
  * Quais operações são permitidas (Create, Update, Delete)?  
  * Quais campos são somente leitura?  
  * Quais validações (verificar dados) e determinações (calcular dados) devem rodar?  
  * Quais **Ações** customizadas (ex: "Aprovar Pedido") o usuário pode executar?

#### **Camada 2: Business Services Provisioning (A Exposição)**

Onde decidimos quais partes do modelo serão visíveis para o mundo externo e como elas serão agrupadas.

* **Service Definition:** Atua como uma fachada ou "whitelist". Imagine que seu modelo de dados tem 50 entidades interligadas. Para um App específico de "Aprovação", você quer expor apenas o "Cabeçalho do Pedido" e os "Itens". A Service Definition cria esse subconjunto, garantindo segurança e foco.  
* **Service Binding:** É o tradutor técnico. Ele pega a Service Definition abstrata e a vincula a um protocolo de comunicação real. É aqui que você escolhe se o serviço será **OData V2** (para compatibilidade com apps legados) ou **OData V4** (para novos apps Fiori Elements), e se será um serviço de **UI** ou uma **Web API** pura.

#### **Camada 3: Service Consumption (O Consumo)**

Quem usa o serviço final. O RAP é agnóstico quanto ao consumidor, mas brilha com:

* **SAP Fiori Elements:** Framework que lê os metadados e anotações do CDS para gerar interfaces de usuário (List Reports, Object Pages) dinamicamente, sem código JavaScript manual.  
* **Web APIs:** Consumo por sistemas externos (integrações A2A/B2B) ou por interfaces customizadas (Freestyle SAPUI5, React, Angular).

### **3. Managed vs. Unmanaged: A Grande Decisão**

Ao criar um comportamento (Behavior Definition), o desenvolvedor deve escolher o "cenário de implementação". Essa decisão define quem é responsável pelo ciclo de vida da transação.

* **Managed (Gerenciado):** O "Piloto Automático".  
  * **Como funciona:** O framework RAP assume o controle total das operações padrão CRUD. Ele gera o SQL de INSERT, UPDATE e DELETE automaticamente.  
  * **Papel do Desenvolvedor:** Você foca exclusivamente na lógica de negócio específica (validações, cálculos de campos, ações de status). Não precisa se preocupar com buffers, bloqueios (ENQUEUE) ou commits.  
  * **Uso Ideal:** **Novos Desenvolvimentos (Greenfield)** onde as tabelas são novas e não existem lógicas legadas complexas.  
* **Unmanaged (Não Gerenciado):** O "Modo Manual".  
  * **Como funciona:** O framework delega a responsabilidade da persistência para o desenvolvedor. Você deve implementar métodos específicos (CREATE, UPDATE, DELETE, SAVE) em classes ABAP locais.  
  * **Papel do Desenvolvedor:** Você deve escrever o código que chama BAPIs existentes, funções de atualização ou métodos de classes legadas para salvar os dados. Você também gerencia o buffer transacional.  
  * **Uso Ideal:** **Sistemas Legados (Brownfield)**. Essencial quando você precisa construir um App Fiori moderno sobre uma lógica de negócio existente (ex: BAPI de Criação de Ordem de Venda) que é complexa demais para ser reescrita do zero.

### **4. O Ciclo de Vida do Desenvolvimento (Workflow Detalhado)**

Para criar um App Fiori do zero no ADT (Eclipse), seguimos um fluxo rigoroso:

1. **Database Table:** Criar a tabela física no Dicionário ABAP. No RAP, recomenda-se o uso de UUIDs como chaves primárias e campos de controle administrativo padrão.  
2. **Interface View (CDS):** Criar a visão básica (Basic View) diretamente sobre a tabela. Esta visão deve ser "limpa", renomeando campos técnicos para nomes semânticos (CamelCase).  
3. **Projection View (CDS):** Criar uma visão de consumo (Consumption View) específica para a aplicação. Aqui filtramos campos sensíveis e preparamos os dados para a UI.  
4. **Metadata Extension:** Criar um arquivo separado (.ddlx) para adicionar anotações de UI (ex: @UI.lineItem, @UI.selectionField). Isso mantém a CDS View limpa, separando a lógica de dados da lógica de apresentação.  
5. **Behavior Definition (BDEF):** Definir as capacidades transacionais na Interface View e projetá-las na Projection View. É aqui que o compilador cria as classes de pool de comportamento (Behavior Pools) onde escreveremos nosso código ABAP.  
6. **Service Definition:** Agrupar as Projection Views relevantes em um serviço nomeado.  
7. **Service Binding:** Publicar o serviço localmente (/sap/opu/odata...). No ambiente Cloud, é necessário "Publicar" explicitamente para ativar o endpoint.  
8. **Preview:** Utilizar a função de *Preview* do Service Binding para testar a aplicação Fiori Elements instantaneamente no navegador, validando anotações e comportamentos.

### **5. Diagrama Conceitual (Fluxo de Dados)**

      [ Browser / Fiori Launchpad ]  
                  ^  
                  | (HTTPS / OData)  
                  v  
      [ SAP Gateway / Service Binding ]  
                  ^  
                  |  
      [ Service Definition (Projeção) ]  
                  ^  
                  |  
[ Projection CDS Views ] <--- [ Behavior Projection ]  
          ^                            ^  
          | (Herança)                  | (Delegação)  
          |                            v  
 [ Interface CDS Views ] <--- [ Behavior Definition (IMPLEMENTAÇÃO) ]  
          ^                            ^  
          | (SQL)                      | (ABAP EML)  
          v                            v  
 [ Banco de Dados HANA ]      [ Lógica de Negócio (Classes BP_) ]

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **RAP (RESTful ABAP Programming Model):** Arquitetura moderna da SAP para construção de serviços OData e aplicações Fiori no S/4HANA e BTP, baseada em CDS e Behavior Definitions.  
* **CDS (Core Data Services):** Linguagem de definição de dados (DDL) avançada. No RAP, é usada para definir a estrutura dos dados (Entidades), suas associações e semântica rica (anotações).  
* **BDEF (Behavior Definition):** Artefato ABAP que define as capacidades transacionais de uma entidade CDS (Criação, Edição, Bloqueio, Ações, Validações). É o contrato de comportamento do Business Object.  
* **Service Definition:** Artefato que projeta um modelo de dados para um escopo específico (Caso de Uso), listando quais entidades serão expostas no serviço.  
* **Service Binding:** Artefato que vincula a Service Definition a um protocolo específico (OData V2/V4) para ser consumido via HTTP. Define o endpoint final da API.  
* **Greenfield:** Termo usado para projetos novos, iniciados do zero, sem legado. Geralmente usa RAP Managed, permitindo desenvolvimento rápido e padronizado.  
* **Brownfield:** Termo usado para projetos em sistemas existentes com muito código legado. Geralmente usa RAP Unmanaged para reaproveitar BAPIs e lógicas de negócio validadas ao longo de anos.  
* **EML (Entity Manipulation Language):** Extensão da linguagem ABAP usada dentro das classes de comportamento para manipular os Business Objects do RAP (MODIFY, READ, COMMIT).

#### **Pontos de Atenção (Arquitetura)**

| Conceito | Função Principal | Camada | Analogia |
| :---- | :---- | :---- | :---- |
| **CDS View** | Modelagem de Dados | Data Modeling | O "Esqueleto" e os "Músculos" |
| **Behavior** | Lógica Transacional | Business Logic | O "Cérebro" (Decisões e Regras) |
| **Service Def.** | Seleção de Escopo | Service Provisioning | O "Cardápio" (O que está disponível) |
| **Service Binding** | Protocolo de Comunicação | Service Provisioning | O "Garçom" (Entrega o pedido em OData) |

### **📝 Quiz de Fixação**

Q1: Em um cenário "Managed" (Gerenciado) do RAP, quem é responsável por executar as operações SQL de INSERT, UPDATE e DELETE no banco de dados e gerenciar o bloqueio (Locking)?  
R: O próprio framework RAP. O desenvolvedor não precisa escrever código para as operações CRUD padrão nem para o gerenciamento de bloqueios (Enqueues), focando apenas em lógicas de negócio específicas (determinations/validations) e ações.  
Q2: Qual é a função primordial de uma "Service Definition" e como ela difere do "Service Binding"?  
R: A Service Definition define o escopo lógico do serviço, listando quais CDS Views e Entidades serão expostas (o "quê"). Ela é agnóstica de protocolo. O Service Binding pega essa definição e aplica um protocolo técnico (OData V2 ou V4) e um tipo de uso (UI ou Web API), criando o endpoint acessível (o "como").  
Q3: Se eu tenho uma BAPI antiga complexa que calcula impostos, realiza verificações de crédito e salva pedidos, e preciso usá-la em um novo App Fiori, qual tipo de implementação RAP devo escolher: Managed ou Unmanaged?  
R: Unmanaged. Como a lógica de gravação e validação profunda já existe na BAPI legada e seria arriscado ou custoso reescrevê-la, o cenário Unmanaged permite que você delegue o salvamento para esse código existente, atuando como um "wrapper" moderno para o legado.