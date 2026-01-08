# **Módulo 01: Programação ABAP Básica**

## **Aula 01: Getting Started (Iniciando a Jornada)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Compreender profundamente a mudança de paradigma do ABAP Clássico para o **ABAP Cloud** e suas implicações na arquitetura de sistemas.  
2. Identificar e configurar o ambiente de desenvolvimento moderno: **ABAP Development Tools (ADT)** no Eclipse, entendendo suas vantagens sobre a SE80.  
3. Entender a hierarquia de organização de software: Componentes de Software, Pacotes e o papel do **abapGit**.  
4. Criar, ativar e executar a primeira aplicação ABAP ("Hello World") utilizando uma Classe Global e a interface de Console, abandonando o conceito de Reports tradicionais.

### **1\. Introdução ao Ambiente de Desenvolvimento Moderno**

No ecossistema de desenvolvimento SAP tradicional, a transação **SE80 (Object Navigator)** dentro do SAP GUI reinou soberana por décadas. No entanto, com a evolução para o **SAP BTP (Business Technology Platform)** e o **SAP S/4HANA Cloud**, o desenvolvimento mudou para uma abordagem mais aberta e padronizada pela indústria.

A ferramenta padrão agora é o **ADT (ABAP Development Tools)**, um conjunto de plugins oficiais da SAP instalados sobre a plataforma Eclipse.

#### **Por que abandonar o SAP GUI e adotar o ADT?**

A transição para o Eclipse não é apenas estética; é funcional e necessária para o modelo de nuvem.

* **Velocidade e Refatoração:** O ADT oferece ferramentas de refatoração poderosas que não existem no SAP GUI. Você pode renomear métodos em todo o sistema, extrair constantes ou variáveis locais com um clique e formatar código automaticamente ("Pretty Printer" avançado).  
* **Suporte Exclusivo a Funcionalidades Cloud:** O desenvolvimento de **Core Data Services (CDS Views)**, **Behavior Definitions (RAP)** e a nova sintaxe de Service Binding são suportados **apenas** no ADT. Não é possível editar esses objetos via SAP GUI.  
* **Múltiplas Conexões e Projetos:** Diferente do SAP GUI, onde você está logado em um mandante por vez por janela, o ADT permite visualizar e comparar códigos de diferentes sistemas (ex: Desenvolvimento vs. Qualidade) lado a lado na mesma interface.  
* **Integração com Ferramentas Modernas:** O ADT integra-se nativamente com o **abapGit** (para versionamento de código descentralizado) e pipelines de CI/CD (Integração e Entrega Contínuas), fundamentais para práticas de DevOps.

### **2\. O Conceito de ABAP Cloud e Clean Core**

Antes de escrever qualquer linha de código, é crucial entender a filosofia por trás do **ABAP Cloud**. Este não é apenas "ABAP na Nuvem", mas sim um modelo de governança e restrição de linguagem.

#### **O Problema do Legado ("Spaghetti Code")**

No ABAP Clássico, os desenvolvedores tinham "superpoderes" perigosos. Podiam ler qualquer tabela (mesmo as de configuração interna do SAP), modificar o comportamento padrão via modificações de núcleo e acessar o sistema operacional.

* **Consequência:** Quando a SAP lançava uma atualização (Upgrade), esses códigos personalizados quebravam, pois dependiam de estruturas internas que mudavam. Isso tornava os projetos de upgrade caros e demorados (o famoso "inferno da SPDD/SPAU").

#### **A Solução: Clean Core (Núcleo Limpo)**

O **Clean Core** é a estratégia para garantir que o sistema ERP possa ser atualizado automaticamente (como seu smartphone atualiza o Android/iOS) sem quebrar as customizações. Para isso, o **ABAP Cloud** impõe restrições técnicas rigorosas:

1. **Language Version 5 (ABAP for Cloud Development):** O compilador bloqueia comandos obsoletos ou perigosos. Você não pode usar CALL SCREEN (Dynpros), WRITE (Listas clássicas), ou acesso direto a arquivos do servidor.  
2. **Released Objects (Objetos Liberados):** Esta é a "Regra de Ouro". Você só pode referenciar objetos SAP que foram explicitamente marcados como **APIs Públicas (Whitelisted)**.  
   * *Exemplo:* No clássico, líamos a tabela MARA para dados de material. No Cloud, isso gera erro de sintaxe. Devemos usar a CDS View pública I\_Product, que é a "interface contrato" estável que a SAP garante que não mudará.  
3. **Acesso a Dados:** O acesso direto ao banco de dados é restrito. Toda leitura deve passar por CDS Views liberadas, garantindo que as verificações de segurança e a lógica de negócio sejam respeitadas.

### **3\. Estrutura de Organização do Código**

A organização do código no ABAP moderno é mais rígida para facilitar o transporte e o desacoplamento.

#### **1\. Software Component (Componente de Software)**

É o contêiner de nível mais alto. No ambiente On-Premise, é gerenciado via transações de transporte (SE01/SE09/STMS). No ambiente Cloud/BTP, o Componente de Software é frequentemente vinculado a um repositório **Git**. Isso permite que o código seja gerenciado com *branches*, *pull requests* e *code reviews* externos.

#### **2\. Package (Pacote)**

O Pacote funciona como uma "pasta" ou "namespace", mas no ABAP moderno ele tem uma função vital de **Encapsulamento**.

* **Package Interface:** Um pacote pode definir quais de seus objetos são visíveis para outros pacotes.  
* **Use Access:** Um pacote consumidor deve declarar explicitamente que usa a interface do pacote provedor.  
* Isso impede que desenvolvedores usem classes ou tabelas "internas" de outros módulos indevidamente, forçando uma arquitetura limpa.  
* Todo objeto ABAP (Classes, Tabelas, CDS) deve pertencer a um pacote. Objetos locais ($TMP) não são transportáveis.

### **4\. O Primeiro Programa: "Hello World" Moderno**

Esqueça os comandos REPORT e WRITE. No ABAP Cloud, a lógica de apresentação (UI) é totalmente separada da lógica de backend. Não existem telas geradas pelo servidor ABAP (Dynpros).

Para testar lógica de backend, utilizamos uma **Classe ABAP Global** que implementa uma interface especial: if\_oo\_adt\_classrun.

#### **Por que uma Interface?**

A interface if\_oo\_adt\_classrun funciona como um "contrato". Ela garante que sua classe tenha um método main que o ambiente ADT sabe chamar. É o equivalente ao public static void main do Java ou C\#.

#### **Passo a Passo Detalhado**

1. No ADT, clique com botão direito no seu Pacote \> **New** \> **ABAP Class**.  
2. Nomeie como zcl\_hello\_world (ou prefixo do seu usuário) e adicione uma descrição.  
3. Na aba de Interfaces, adicione if\_oo\_adt\_classrun.  
4. Ative a classe (Ctrl+F3).  
5. Execute a classe pressionando F9.

#### **Análise do Código (Syntax Highlighting e Comentários)**

CLASS zcl\_hello\_world DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    " A interface if\_oo\_adt\_classrun marca esta classe como executável pelo console do Eclipse.  
    " Sem ela, não podemos rodar a classe diretamente com F9.  
    INTERFACES if\_oo\_adt\_classrun .  
      
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_hello\_world IMPLEMENTATION.

  " Implementação do método MAIN da interface.  
  " Este é o ponto de entrada quando a classe é executada.  
  METHOD if\_oo\_adt\_classrun\~main.  
    
    " O objeto 'out' é uma instância injetada automaticamente pelo framework.  
    " Ele possui o método 'write', que substitui o antigo comando WRITE do ABAP Clássico.  
    " Isso envia o texto para a aba 'Console' no Eclipse.  
    out-\>write( 'Hello World\! Bem-vindo ao ABAP Moderno.' ).  
      
    " Exemplo de uso de uma API liberada (Released Object)  
    " cl\_abap\_context\_info é a classe padrão para obter dados do sistema (data, hora, usuário)  
    " substituindo as variáveis de sistema 'sy-datum' ou 'sy-uzeit' em muitos casos.  
    DATA(lv\_date) \= cl\_abap\_context\_info=\>get\_system\_date( ).  
      
    " Uso de String Templates (|...|) para concatenação moderna  
    out-\>write( |A data de hoje no servidor é: { lv\_date DATE \= ISO }| ).

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

Esta seção destaca os termos-chave e conceitos fundamentais para alimentar o gerador de Flashcards do NotebookLM e facilitar sua revisão.

#### **Glossário Técnico Expandido**

* **ADT (ABAP Development Tools):** IDE baseada em Eclipse, mandatória para desenvolvimento ABAP moderno (RAP, CDS, Cloud). Substitui a SE80.  
* **ABAP Cloud:** Modelo de desenvolvimento restrito focado em "Clean Core". Proíbe acesso direto ao sistema e obriga o uso de APIs liberadas.  
* **Clean Core:** Estratégia arquitetural da SAP para manter o núcleo do ERP livre de modificações diretas, garantindo que upgrades de software não quebrem extensões customizadas.  
* **Released Object (Objeto Liberado):** Artefatos SAP (Tabelas, Classes, CDS) marcados com um contrato de estabilidade (C1/C2). Apenas estes objetos podem ser usados em desenvolvimento ABAP Cloud.  
* **if\_oo\_adt\_classrun:** Interface padrão para criar classes executáveis via console no ADT. Substitui a necessidade de criar Reports (SE38) para testes de lógica.  
* **String Templates (|...|):** Sintaxe moderna para manipulação de strings que permite interpolação de variáveis e formatação embutida dentro de barras verticais.  
* **abapGit:** Cliente Git para ABAP, permitindo importação/exportação de código e versionamento distribuído. Essencial para ambientes Cloud e BTP.

#### **Pontos de Atenção: O Que Mudou? (Clássico vs. Moderno)**

| Recurso | ABAP Clássico (Legacy/On-Premise) | ABAP Moderno (Cloud/RAP) |
| :---- | :---- | :---- |
| **IDE Principal** | SAP GUI (SE80, SE38, SE11) | Eclipse com ADT |
| **Saída de Texto** | Comando WRITE 'Texto'. | Método out-\>write( 'Texto' ). |
| **Tipo de Programa** | Report (REPORT z...) | Classe Global com if\_oo\_adt\_classrun |
| **Leitura de Dados** | SELECT \* FROM tabela\_sap (Qualquer tabela) | SELECT \* FROM cds\_view\_liberada (Apenas liberadas) |
| **Telas (UI)** | Dynpro / Web Dynpro | SAP Fiori (UI5 / Fiori Elements) |
| **Variáveis Sistema** | Uso livre de sy-datum, sy-uname | Uso de classes como cl\_abap\_context\_info |

### **📝 Quiz de Fixação (Sugestão para o NotebookLM)**

Q1: Por que o comando WRITE e a criação de telas Dynpro não são suportados no modelo de desenvolvimento ABAP Cloud?  
R: O ABAP Cloud separa estritamente o Backend do Frontend. O Backend (ABAP) deve fornecer apenas serviços e APIs (OData), enquanto o Frontend deve ser baseado em tecnologias web (SAP Fiori/UI5). Comandos como WRITE geram HTML legado no servidor, o que viola essa arquitetura e não é compatível com a nuvem.  
Q2: Um desenvolvedor tenta ler a tabela MARA (Mestre de Materiais) em um ambiente S/4HANA Cloud e recebe um erro de sintaxe. Qual é a causa e a solução?  
R: A causa é que a tabela MARA não é um "Released Object" no modelo ABAP Cloud. O acesso direto a tabelas físicas internas é proibido para garantir o Clean Core. A solução é encontrar e utilizar a CDS View pública equivalente liberada pela SAP, como a I\_Product.  
Q3: Qual é a função da interface if\_oo\_adt\_classrun e por que ela é usada no lugar de Reports tradicionais?  
R: Ela permite que uma classe global seja executada diretamente pelo console do ADT (Eclipse). É usada no lugar de Reports porque no ABAP Cloud não existem telas de seleção ou saída de lista clássica; a interface fornece uma maneira leve e padronizada de testar lógica de backend e exibir resultados simples.