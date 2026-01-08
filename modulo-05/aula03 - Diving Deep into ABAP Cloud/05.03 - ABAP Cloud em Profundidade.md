# **Módulo 05: Praticando a Extensibilidade Clean Core**

## **Aula 03: ABAP Cloud em Profundidade**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar claramente o produto comercial "S/4HANA Cloud" do modelo de governança técnica **ABAP Cloud**, entendendo como aplicar este último em ambientes On-Premise e Private Cloud.  
2. Configurar e validar a propriedade **ABAP Language Version 5 (ABAP for Cloud Development)** em classes, interfaces e outros objetos de repositório, compreendendo as restrições sintáticas impostas pelo compilador.  
3. Identificar, buscar e utilizar **Released Objects** (APIs Liberadas) através da árvore de objetos do ADT, garantindo que o código customizado dependa apenas de contratos estáveis da SAP.  
4. Compreender a taxonomia dos **Contratos de Estabilidade** (C0, C1, C2, C3) e como eles ditam o ciclo de vida e o escopo de uso dos objetos.  
5. Planejar e implementar a arquitetura de **Wrappers** (Classes Envelope) para encapsular código legado não liberado, permitindo sua reutilização segura em um ambiente Clean Core.

### **1\. O que é "ABAP Cloud"? Desfazendo a Confusão**

Existe uma confusão terminológica comum no mercado: muitos acreditam que "ABAP Cloud" se refere apenas ao desenvolvimento feito na plataforma SAP BTP (Business Technology Platform) ou na edição S/4HANA Cloud Public Edition. Isso não é verdade.

Definição Precisa:  
O ABAP Cloud não é um produto que você compra; é um Modelo de Desenvolvimento. É um conjunto de regras, restrições de linguagem e melhores práticas arquiteturais projetadas para criar extensões "Clean Core".

* **Abrangência:** Este modelo é o único permitido na Public Cloud, mas é **altamente recomendado** (e totalmente suportado) nas edições **S/4HANA Cloud Private Edition** e **S/4HANA On-Premise** (a partir da versão 2022).  
* **O Objetivo:** Desenvolver em "ABAP Cloud" significa escrever código que é **Upgrade-Safe** (Seguro para Atualização) e **Cloud-Ready** (Pronto para a Nuvem). Mesmo que seu servidor esteja hoje no seu data center local, escrever em ABAP Cloud garante que uma futura migração para a nuvem pública seja suave, pois seu código já respeita as restrições de isolamento e segurança.

Para programar neste modelo, a propriedade técnica **ABAP Language Version** do seu objeto (Classe, Interface, CDS View) deve estar explicitamente definida como **ABAP for Cloud Development (5)**.

### **2\. O Que Mudou na Sintaxe? (Language Version 5\)**

Quando você define a versão da linguagem para "Cloud Development" nas propriedades do objeto no Eclipse, você está instruindo o compilador ABAP a ativar o **Strict Mode** (Modo Estrito). O compilador passa a atuar como um "porteiro", bloqueando ativamente qualquer comando que viole os princípios de segurança, performance ou desacoplamento da nuvem.

#### **❌ O que foi banido? (E por quê?)**

A lista de proibições não é arbitrária; ela visa eliminar dívidas técnicas históricas e riscos de segurança.

* **Telas Clássicas (Dynpro / Web Dynpro):** Comandos como CALL SCREEN, MODULE, PAI/PBO são proibidos. A nuvem exige desacoplamento total entre Backend e Frontend. A única interface permitida é via protocolos web (HTTP/OData), consumidos por SAP Fiori ou apps externos.  
* **Acesso ao Sistema Operacional:** Comandos como OPEN DATASET, CALL 'SYSTEM', ou acesso direto ao Kernel são bloqueados. Em um ambiente de nuvem multitenant, permitir que um desenvolvedor acesse o sistema de arquivos do servidor é uma falha de segurança crítica.  
* **Tabelas SAP Diretas:** O acesso direto (SELECT, UPDATE) a tabelas físicas padrão (como MARA, VBAK, T001) é restrito. Isso previne que o código do cliente quebre se a SAP decidir mudar a estrutura interna do banco de dados (por exemplo, encurtar um campo ou mudar uma chave).  
* **Geração Dinâmica de Código:** Comandos como GENERATE SUBROUTINE POOL e INSERT REPORT são proibidos devido à dificuldade de auditar e garantir a segurança do código gerado em tempo de execução.

#### **✅ O Que é Permitido e Encorajado:**

* **ABAP SQL Moderno:** Uso intensivo de SELECT com variáveis host (@), expressões SQL (CASE, CAST), e funções de agregação.  
* **Objetos RAP:** Criação de CDS Views para modelagem e Behavior Definitions para transações.  
* **Classes OO:** A Orientação a Objetos é mandatória. Subrotinas procedurais (FORM/PERFORM) e Function Modules (para criação) são obsoletos neste modelo.  
* **Tipos e Constantes Globais:** Uso de Interfaces e Classes para definir constantes, em vez de Type Groups obsoletos.

### **3\. Released Objects: A "Lista Branca" (Whitelisting)**

No paradigma Clean Core, o acesso ao código da SAP deixa de ser uma "porta aberta" e passa a ser controlado por uma **Whitelist** (Lista Branca). Você só pode utilizar objetos que a SAP garantiu contratualmente que são estáveis. Estes são chamados de **Released Objects** (Objetos Liberados).

#### **Como Identificar um Objeto Liberado?**

No **ABAP Development Tools (ADT)**, você não precisa adivinhar.

1. **Project Explorer:** Navegue até a pasta virtual Released Objects. Ela organiza todos os artefatos liberados por tipo (Classes, Interfaces, CDS Views, BAdIs).  
2. **Verificação Individual:** Abra qualquer objeto standard (ex: uma classe CL\_...). Vá na aba **Properties** \-\> **API State**.  
3. **Status:** Se estiver marcado como "Released", você verá para quais contratos ele é válido (ex: "Use in Cloud Development"). Se estiver "Not Released", o uso gerará erro de sintaxe no seu código ABAP Cloud.

#### **Entendendo os Contratos de Estabilidade (C-Contracts)**

A SAP classifica a liberação em níveis de contrato, definindo *como* você pode usar o objeto:

* **C0 \- Contract for Extensibility:** O objeto pode ser estendido. Exemplo: Uma CDS View ou Tabela que permite adicionar campos customizados (Extension Includes).  
* **C1 \- Contract for System-Internal Use:** A API pública mais comum. O objeto (Classe, Interface, CDS) pode ser chamado ou selecionado pelo seu código ABAP customizado dentro do mesmo sistema. Garante que a assinatura do método ou os campos da view não mudarão.  
* **C2 \- Contract for Remote Use:** O objeto é liberado para ser consumido externamente via OData ou RFC por sistemas side-by-side (BTP) ou apps externos.  
* **C3 \- Contract for Configuration:** (Menos comum para devs) Objetos de configuração estáveis.

### **4\. A Estratégia dos "Wrappers" (A Ponte no Private Cloud)**

Aqui reside o segredo para sobreviver no **S/4HANA Private Cloud** ou em migrações Brownfield. Frequentemente, você precisará usar uma funcionalidade antiga (ex: uma BAPI de cálculo de impostos específica do Brasil, ou uma Função Z antiga crítica) que a SAP ainda não liberou para o modelo Cloud (não tem contrato C1).

Se você tentar chamar essa função diretamente do seu código "Tier 2" (ABAP Cloud), receberá um erro.

**A Solução Arquitetural: O Padrão Wrapper**

Nós criamos uma camada de isolamento. Como no Private Cloud temos acesso ao "Tier 3" (ABAP Clássico/Não Restrito), usamos isso a nosso favor.

1. **Crie o Wrapper (Tier 3):** Crie uma Classe ABAP no modo "Standard ABAP" (sem restrição de Cloud).  
   * Esta classe tem "superpoderes": ela pode chamar a BAPI antiga, ler a tabela MARA ou usar qualquer código legado.  
2. **Exponha uma Interface Limpa:** O método público desta classe wrapper deve receber e retornar apenas tipos de dados simples ou liberados.  
3. **Libere o Wrapper (Release C1):** Aqui está o "pulo do gato". Você, como desenvolvedor, marca sua própria classe Wrapper como **"Released for Cloud Development"** (Contrato C1). Você está atestando que essa interface é estável.  
4. **Consuma no Tier 2:** Agora, seu código ABAP Cloud (seu App Fiori moderno) pode instanciar e chamar sua classe Wrapper.

*Nota:* O Wrapper atua como uma "zona de descontaminação". Ele esconde a complexidade e a "sujeira" do legado, permitindo que o novo desenvolvimento permaneça limpo e compliant com as regras da nuvem.

### **5\. Exemplo Prático: Erro de Compilação vs Solução**

Vamos visualizar a diferença prática entre o código legado e o código moderno ao tentar ler dados básicos de um material.

**Cenário:** Precisamos buscar o Grupo de Mercadorias de um Material.

Código Proibido (Erro no ABAP Cloud):  
O compilador bloqueará este código porque MARA é um detalhe de implementação interna do SAP ERP, não uma API pública.  
" Erro de Sintaxe: The object 'MARA' is not released for cloud development.  
SELECT matnr, matkl   
  FROM mara   
  INTO TABLE @lt\_data  
  WHERE matnr \= @lv\_material.

Código Correto (Clean Core):  
Utilizamos a CDS View de Interface I\_Product, que é a "Fachada Pública" garantida pela SAP para dados de produtos.  
" Correto: I\_Product é uma CDS View liberada com contrato C1.  
SELECT Product as MaterialID, ProductGroup  
  FROM I\_Product  
  INTO TABLE @lt\_data  
  WHERE Product \= @lv\_material.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **ABAP Cloud:** Modelo de desenvolvimento e governança que impõe o uso de tecnologias modernas (RAP, CDS) e restringe o acesso a objetos legados, visando a construção de extensões "Clean Core" e prontas para upgrade.  
* **Language Version 5:** Configuração técnica do compilador ABAP ("ABAP for Cloud Development"). Quando ativada em um objeto, o compilador verifica cada linha de código contra uma lista de comandos permitidos e objetos liberados, gerando erros para qualquer violação.  
* **Released Object (Objeto Liberado):** Um artefato SAP (Classe, CDS, Interface, BAdI) que foi explicitamente marcado pela SAP como estável para uso em desenvolvimento customizado. Possui um "API State" visível no ADT e garantia de compatibilidade futura.  
* **Wrapper (Classe Envelope):** Um padrão de design estrutural usado para encapsular funcionalidade legada (não liberada) dentro de uma interface moderna e liberada. No contexto S/4HANA Private Cloud, permite que código ABAP Cloud (Tier 2\) acesse funcionalidades do ABAP Clássico (Tier 3\) de forma controlada.  
* **Tier 2 (Developer Extensibility):** Camada de desenvolvimento onde aplicamos o modelo ABAP Cloud. O código roda no mesmo stack do S/4HANA (acesso local a dados), mas é isolado logicamente pelas regras da linguagem, garantindo que não "quebre" o núcleo.

#### **Tabela de Conversão: Legado vs. Cloud (De/Para)**

| Conceito de Negócio | Objeto Legado (Evitar/Proibido) | Objeto Liberado (Recomendado/Cloud) |
| :---- | :---- | :---- |
| **Mestre de Clientes** | Tabela KNA1 / KNB1 | CDS View I\_Customer |
| **Mestre de Materiais** | Tabela MARA / MARC | CDS View I\_Product |
| **Cabeçalho de Pedido** | Tabela VBAK | CDS View I\_SalesOrder |
| **Dados do Usuário** | Tabela USR02 / sy-uname | Classe CL\_ABAP\_CONTEXT\_INFO |
| **Data e Hora do Sistema** | Variáveis sy-datum / sy-uzeit | Classe CL\_ABAP\_CONTEXT\_INFO |
| **Mensagens de Erro** | MESSAGE ... TYPE 'E' | Classes de Exceção com IF\_T100\_MESSAGE |

### **📝 Quiz de Fixação**

Q1: O que acontece se eu tentar usar o comando CALL SCREEN 100 dentro de uma classe configurada com a propriedade "ABAP Language Version 5"?  
R: O código não será ativado e o compilador gerará um erro de sintaxe fatal. O comando CALL SCREEN pertence à tecnologia Dynpro clássica (SAP GUI), que não é suportada no modelo ABAP Cloud, pois este exige uma separação estrita entre backend e frontend (Fiori).  
Q2: Como um desenvolvedor pode descobrir se uma determinada CDS View padrão da SAP (ex: I\_BillingDocument) pode ser usada legalmente no seu desenvolvimento ABAP Cloud?  
R: Abrindo a view no ABAP Development Tools (ADT) no Eclipse, acessando a aba Properties e verificando o campo API State. Se estiver marcada como "Released" e possuir o contrato C1 (Use in Cloud Development), ela pode ser usada. Se estiver "Not Released", seu uso gerará erro de compilação.  
Q3: Em um cenário de S/4HANA Private Cloud, você precisa reutilizar uma função Z complexa antiga que não segue as regras do ABAP Cloud. Qual é a estratégia arquitetural recomendada para acessá-la a partir de um novo App Fiori RAP?  
R: Deve-se criar um Wrapper. Ou seja, criar uma classe intermediária na camada de ABAP Clássico (Tier 3\) que chama a função Z antiga. Em seguida, deve-se liberar (Release C1) essa classe Wrapper explicitamente para uso em Cloud Development. O App Fiori (Tier 2\) chamará então o Wrapper, que por sua vez chamará o código legado.