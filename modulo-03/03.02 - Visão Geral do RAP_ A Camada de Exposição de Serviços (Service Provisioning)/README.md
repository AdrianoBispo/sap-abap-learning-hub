# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 02: Visão Geral do RAP: A Camada de Exposição de Serviços (Service Provisioning)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Identificar e articular o papel crítico da camada de **Business Service Provisioning** como a ponte tradutora entre o modelo de dados ABAP (CDS) e o mundo exterior (HTTP/OData).  
2. Criar e estruturar uma **Service Definition** para delimitar o escopo funcional da aplicação, aplicando técnicas de *Aliasing* para desacoplar nomes técnicos de nomes públicos.  
3. Configurar estrategicamente um **Service Binding**, escolhendo entre os protocolos OData V2 e OData V4 e definindo o tipo de cenário (UI ou Web API) adequado aos requisitos do projeto.  
4. Compreender o ciclo de vida de publicação de um serviço no SAP Gateway e utilizar a ferramenta de **Preview** para validação rápida de metadados e comportamento de UI.

### **1. Revisitando a Arquitetura RAP para UI**

No Módulo 1, fomos introduzidos às três camadas do RAP. Agora, nosso foco recai exclusivamente sobre a camada intermediária: **Business Service Provisioning**.

Esta camada é o ponto de inflexão onde o desenvolvimento deixa de ser "puramente ABAP" e passa a ser "orientado a serviços web". Sem essa camada, seus modelos de dados (CDS Views) e comportamentos (BDEF) são apenas lógicas internas inalcançáveis pelo navegador ou por sistemas externos.

O Fluxo de Consumo Detalhado:  
Navegador (Fiori Elements) <--> HTTPS (JSON/XML) <--> Service Binding (Protocolo) <--> Service Definition (Escopo) <--> Consumption View  
Por que separar Definição de Binding?  
Essa separação permite reutilização e flexibilidade.

* Você pode ter **uma** *Service Definition* (o escopo do negócio).  
* E criar **dois** *Service Bindings* para ela: um Binding V2 para um app legado e um Binding V4 para um novo app Fiori, ambos consumindo a mesma definição sem duplicar código.

### **2. Service Definition: O "Cardápio" do Serviço**

A **Service Definition** é o artefato de projeção onde selecionamos explicitamente *quais* CDS Views e Entidades farão parte do contrato do nosso serviço. Ela atua como uma **Whitelist** (Lista Branca) de segurança e organização.

* **O Problema do Excesso:** Imagine que seu modelo de dados de "Vendas" possui 50 CDS Views interconectadas (Cabeçalho, Item, Divisão de Remessa, Parceiro, Endereço, etc.). Se você expusesse tudo automaticamente, o serviço ficaria pesado, confuso e inseguro.  
* **A Solução (Escopo):** Para um App de "Aprovar Viagens", você expõe apenas Travel e Customer. Para um App de "Auditoria", você expõe Travel, Booking e Log. A Service Definition permite criar múltiplas "janelas" para o mesmo modelo de dados subjacente.

A Arte do Aliasing (Renomeação):  
Uma prática recomendada é usar o AS Alias para desacoplar o nome técnico do ABAP (interno) do nome público da API (externo).

* *Interno:* Z_C_TRAVEL_PROCESS_V2 (Longo, técnico, versionado).  
* Externo: Travel (Limpo, legível, semântico).  
  Isso permite que você refatore o nome da CDS View no futuro sem quebrar a URL que o frontend utiliza.

**Exemplo de Código Expandido:**

@EndUserText.label: 'Serviço para App de Viagens'  
define service Z_UI_TRAVEL_V4 {  
    
  /* Entidade Raiz */  
  expose Z_C_TRAVEL as Travel;  
    
  /* Entidades Associadas e Value Helps */  
  /* É vital expor as entidades que servem como tabelas de texto ou help */  
  expose /DMO/I_Customer as Customer;  
  expose /DMO/I_Agency   as Agency;  
  expose I_Currency      as Currency;  
    
  /* Note: Se Z_C_TRAVEL tem uma associação para _Booking,   
     e queremos que o Fiori navegue para os itens,   
     precisamos expor a view de Booking aqui também, ou garantir  
     que a associação seja acessível via metadados. */  
}

### **3. Service Binding: O Protocolo de Comunicação**

O **Service Binding** é o artefato técnico que pega a Service Definition (abstrata) e a "amarra" a um endpoint HTTP real no servidor SAP. É aqui que as decisões de infraestrutura são tomadas.

#### **Decisão 1: O Protocolo (OData V2 vs V4)**

* **OData V2:** O padrão "clássico". Amplamente suportado por versões antigas do SAPUI5 e ferramentas de integração legadas (SAP PO/PI). Use apenas se tiver restrições de compatibilidade.  
* **OData V4:** O novo padrão para S/4HANA e RAP.  
  * **Performance:** Payload JSON mais enxuto.  
  * **Batching:** Melhor agrupamento de requisições.  
  * **Features:** Suporte nativo a tipos de dados complexos e filtros avançados.  
  * **Recomendação:** Para novos Apps Fiori Elements, **sempre use OData V4**.

#### **Decisão 2: O Tipo de Uso (Binding Type)**

1. **OData V4 - UI (User Interface):**  
   * **Objetivo:** Alimentar aplicações Fiori Elements ou SAPUI5 customizadas.  
   * **Comportamento:** O framework gera um documento de metadados ($metadata) rico, incluindo todas as anotações @UI, @EndUserText e @Consumption.  
   * **Resultado:** O navegador recebe instruções visuais ("Pinte isso de vermelho", "Coloque este campo na posição 10").  
2. **OData V4 - Web API:**  
   * **Objetivo:** Integração entre sistemas (A2A) ou exposição para terceiros (B2B).  
   * **Comportamento:** O framework **remove** as anotações de UI do documento de metadados.  
   * **Por que usar?** Torna o serviço mais leve e rápido. Um sistema externo (como um CRM Salesforce ou um script Python) não precisa saber que a cor do status é vermelha ou que a posição do campo é 10. Ele só quer os dados brutos.

### **4. O Processo de Publicação (Publish)**

No ambiente **ABAP Cloud** (BTP) ou S/4HANA recente, criar o Binding não é o fim. O serviço nasce em estado "Inativo".

**O que acontece ao clicar em "Publish"?**

1. **Geração de Artefatos:** O sistema gera classes de carga (Load Classes) no Gateway.  
2. **Registro ICF:** É criado um nó na árvore do *Internet Communication Framework* (ICF), geralmente sob o caminho /sap/opu/odata4/....  
3. **Ativação de Endpoint:** A URL se torna acessível publicamente (dentro da rede segura).

A Ferramenta de Preview:  
O Service Binding no ADT oferece um botão "Preview" para a entidade raiz.

* Isso lança um aplicativo Fiori Elements genérico no seu navegador padrão.  
* **Valor Estratégico:** Permite o desenvolvimento iterativo rápido (Fail Fast). Você altera uma anotação na CDS View, salva, e dá refresh no Preview para ver o resultado, sem precisar fazer deploy de código frontend no VS Code.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Service Definition:** Artefato RAP que define o limite lógico (escopo) de um serviço, listando quais entidades (CDS Views) serão expostas. Funciona como uma "whitelist" de segurança e organização.  
* **Service Binding:** Artefato RAP que vincula uma Service Definition a um protocolo de comunicação (OData V2/V4) e a um perfil de uso (UI ou Web API), gerando o endpoint técnico.  
* **OData V4 - UI:** Tipo de binding otimizado para interfaces humanas. Inclui anotações de vocabulário UI nos metadados, permitindo que o Fiori Elements renderize telas automaticamente.  
* **OData V4 - Web API:** Tipo de binding otimizado para integração de sistemas. Remove anotações visuais para reduzir o tamanho dos metadados e focar na transferência de dados pura.  
* **Expose ... as Alias:** Sintaxe usada na Service Definition para renomear entidades. Essencial para manter a estabilidade da API pública mesmo se os nomes dos objetos técnicos ABAP (CDS Views) mudarem.  
* **ICF (Internet Communication Framework):** Camada do SAP NetWeaver que permite ao sistema ABAP comunicar-se via protocolos de Internet (HTTP/HTTPS/SMTP). Os serviços OData são publicados como nós ICF.

#### **Diagrama de Decisão: Qual Binding Usar?**

1. **O destino é uma tela Fiori Elements?**  
   * Sim -> **Use OData V4 - UI**.  
2. **O destino é uma integração via SAP CPI, Postman ou sistema externo?**  
   * Sim -> **Use OData V4 - Web API** (ou V2 se o sistema externo for antigo).  
3. **Preciso de compatibilidade com apps Fiori antigos (versão 1.x)?**  
   * Sim -> **Use OData V2 - UI**.

### **📝 Quiz de Fixação**

Q1: Por que não devemos expor diretamente as tabelas do banco de dados na Service Definition, mas sim as Consumption Views?  
R: Porque as tabelas não possuem semântica rica, associações navegáveis ou anotações de UI. Expor Consumption Views (C_Views) garante que a projeção de dados esteja correta (apenas campos necessários), que a segurança (DCL) seja aplicada e que as configurações de interface necessárias para o Fiori estejam presentes.  
Q2: Se eu criar uma Service Definition mas não criar um Service Binding, o serviço estará acessível via URL?  
R: Não. O serviço não existirá tecnicamente para o mundo externo. A Service Definition é apenas uma definição lógica interna; o Service Binding é o artefato que efetivamente cria o endpoint HTTP e registra o serviço no Gateway SAP.  
Q3: Qual a consequência de usar um binding do tipo "Web API" para tentar alimentar um aplicativo Fiori Elements?  
R: O aplicativo Fiori Elements provavelmente funcionará de forma precária ou incompleta. Como o binding "Web API" remove as anotações de UI (@UI.*) dos metadados, o Fiori Elements não saberá como desenhar as colunas, filtros ou facetas, resultando em uma tela em branco ou uma tabela genérica sem configuração visual.