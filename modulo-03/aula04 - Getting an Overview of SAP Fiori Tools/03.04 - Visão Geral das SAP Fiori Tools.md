# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 04: Visão Geral das SAP Fiori Tools**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Identificar e configurar o ambiente de desenvolvimento frontend ideal, compreendendo as diferenças e casos de uso entre **VS Code** (Local) e **SAP Business Application Studio (BAS)** (Cloud).  
2. Utilizar o **Application Generator** para automatizar a criação da estrutura inicial de um projeto Fiori Elements, conectando-o corretamente a um serviço OData.  
3. Explorar e manipular a aplicação visualmente usando o **Page Map**, entendendo como ele facilita a configuração de layouts e a extensão de funcionalidades sem a necessidade de codificação manual propensa a erros.  
4. Aplicar configurações complexas e snippets de código com auxílio do **Guided Development**, acelerando a implementação de requisitos comuns.  
5. Diferenciar claramente quando aplicar configurações no **Backend (ADT)** versus configurações locais no **Frontend (Fiori Tools)**, adotando as melhores práticas de arquitetura.

### **1\. O Que são as SAP Fiori Tools?**

Até este ponto da jornada, nosso trabalho foi focado exclusivamente no Backend (ADT/Eclipse), definindo modelos de dados e comportamentos. No entanto, para que o aplicativo exista "de verdade" no SAP Fiori Launchpad e seja acessível aos usuários finais, ele precisa ser empacotado como um projeto Web (HTML5/JavaScript).

As **SAP Fiori Tools** não são uma única ferramenta, mas um conjunto poderoso de extensões desenvolvidas pela SAP para simplificar e acelerar o desenvolvimento de aplicações Fiori. Elas eliminam a necessidade de escrever manualmente configurações complexas em arquivos JSON e XML, que eram fontes frequentes de erros no passado.

#### **Ambientes de Desenvolvimento Suportados**

* **SAP Business Application Studio (BAS):**  
  * *Tipo:* Ambiente de desenvolvimento baseado em nuvem (Cloud IDE), rodando no SAP BTP.  
  * *Vantagens:* Vem pré-configurado com todas as Fiori Tools, Java, Node.js e conectividade com sistemas SAP S/4HANA (Cloud e On-Premise) via Cloud Connector. É a recomendação oficial da SAP para desenvolvimento Side-by-Side e S/4HANA Cloud.  
  * *Configuração:* Zero setup local; basta abrir o navegador.  
* **Visual Studio Code (VS Code):**  
  * *Tipo:* Editor de código local, open-source e extremamente popular.  
  * *Vantagens:* Familiaridade para desenvolvedores web, performance local, funcionamento offline e vasta biblioteca de extensões de terceiros.  
  * *Requisito:* É necessário instalar manualmente o Node.js e o pacote de extensão "SAP Fiori Tools \- Extension Pack" do marketplace.

### **2\. Gerando o App (Application Generator)**

Esqueça a criação manual de pastas como webapp, controller ou arquivos index.html. O **Application Generator** é um assistente (wizard) que guia você passo a passo na criação do esqueleto de uma aplicação profissional.

Para acessá-lo no VS Code ou BAS, abrimos a paleta de comandos (F1 ou Ctrl+Shift+P) e selecionamos *Fiori: Open Application Generator*.

#### **O Fluxo de Geração Detalhado:**

1. **Generator Selection:** Escolhemos "SAP Fiori Elements". (Também existe a opção Freestyle, mas nosso foco é padronização).  
2. **Floorplan Selection:** Escolhemos "List Report Object Page". Este é o padrão ouro para aplicações transacionais RAP.  
3. **Data Source:**  
   * Aqui conectamos ao nosso sistema S/4HANA.  
   * Selecionamos o **Service Binding** (OData V4 \- UI) que criamos na Aula 02\.  
   * O gerador lê os metadados do serviço para entender as entidades disponíveis.  
4. **Entity Selection:**  
   * **Main Entity:** Escolhemos a entidade raiz (ex: Travel). Isso define o que será listado na primeira tela.  
   * **Navigation Entity:** (Opcional) Define para onde o usuário vai ao clicar na linha (ex: to\_Booking).  
5. **Project Attributes:**  
   * Definimos o nome do projeto, namespace (ex: z.travel), e configurações de deploy.

*Resultado:* O gerador cria toda a estrutura de pastas (webapp, manifest.json, ui5.yaml) e configura o proxy para que você possa rodar o app localmente (npm start) consumindo dados do servidor remoto sem problemas de CORS.

### **3\. O Mapa da Página (Page Map)**

Esta é, sem dúvida, a ferramenta visual mais impactante para quem está começando. Ao clicar com o botão direito no projeto (arquivo webapp) e escolher "Show Page Map", você vê uma representação gráfica do fluxo do seu aplicativo.

Visualização:  
List Report \--\> Object Page \--\> Sub-Object Page  
**Funcionalidades do Page Map:**

* **Configuração de Layout:** Permite adicionar ou remover colunas, seções e campos. Embora a recomendação seja fazer isso no Backend (RAP), o Page Map permite *overrides* locais rápidos via arquivos de anotação XML locais (annotation.xml).  
* **Flexible Programming Model (FPM):** É aqui que você adiciona "extensões" ao padrão.  
  * Adicionar uma **Coluna Customizada** na tabela que exibe um micro-gráfico.  
  * Adicionar uma **Seção Customizada** na Object Page para mostrar um mapa do Google Maps.  
  * Adicionar um **Botão na Toolbar** que chama uma função JavaScript específica.  
* **Navegação:** Configura visualmente como a navegação entre as páginas ocorre, sem precisar editar o roteamento complexo no manifest.json.

### **4\. Guided Development (Desenvolvimento Guiado)**

Desenvolver em Fiori Elements às vezes requer configurações de anotações complexas que são difíceis de memorizar. "Como eu adiciono um botão que abre um PDF?", "Como eu faço uma linha da tabela ficar vermelha se o valor for negativo?".

O **Guided Development** é uma biblioteca de "receitas" e tutoriais interativos integrados à IDE.

**Como funciona:**

1. Você abre o painel e pesquisa por uma intenção: "Add Custom Action".  
2. O guia explica o conceito e mostra um formulário.  
3. Você preenche os parâmetros (IDs, Nomes, Labels).  
4. O guia gera o trecho de código (Snippet) correto (seja JavaScript, XML ou JSON).  
5. Ao clicar em "Apply", ele insere o código automaticamente no arquivo correto do seu projeto.

Isso reduz drasticamente a necessidade de consultar a documentação oficial da SAP a todo momento e previne erros de sintaxe.

### **5\. Backend vs. Local Annotation: Uma Decisão Arquitetural**

Uma dúvida comum de todo desenvolvedor Fiori Elements é: *"Se eu posso configurar a tela (colunas, labels) no Page Map (Local), por que aprendemos a fazer no ADT (Backend) nos módulos anteriores?"*

Esta é uma decisão de arquitetura crítica.

* **Backend (ADT / CDS Views):**  
  * *Escopo:* Global e Reutilizável.  
  * *Vantagem:* Se você define o label "Cliente" no Backend, **todos** os aplicativos que consumirem essa CDS View (outro app Fiori, um relatório analítico, uma API externa) herdarão esse label. Garante consistência em toda a empresa.  
  * *Recomendação:* Use para **90%** das configurações (Estrutura, Tipos, Labels Padrão, Validações).  
* **Local (Fiori Tools / annotation.xml):**  
  * *Escopo:* Específico do Aplicativo.  
  * *Vantagem:* Permite especificidades que só fazem sentido *neste* aplicativo. Exemplo: Neste app específico de "Aprovação Rápida", eu quero esconder a coluna "Data de Criação", mas no app de "Auditoria", ela deve aparecer.  
  * *Recomendação:* Use apenas para *overrides* visuais específicos ou quando a anotação não for suportada no Backend (casos raros no RAP moderno).

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **SAP Business Application Studio (BAS):** Ambiente de desenvolvimento baseado em nuvem (BTP) que vem pré-configurado com todas as Fiori Tools. É a IDE recomendada para o Frontend, oferecendo zero configuração inicial.  
* **VS Code (Visual Studio Code):** Editor de código popular que pode ser transformado em uma IDE Fiori poderosa através da instalação do "SAP Fiori Tools Extension Pack". Requer configuração local de Node.js.  
* **Page Map:** Ferramenta visual das Fiori Tools que exibe a hierarquia de páginas da aplicação, permitindo configurar anotações locais, navegação e extensões de controlador (Flexible Programming Model).  
* **Application Generator:** Assistente (Wizard) que automatiza a criação do esqueleto de uma aplicação SAPUI5/Fiori Elements, conectando-a a um serviço OData e gerando arquivos de configuração vitais (ui5.yaml, manifest.json).  
* **Manifest.json:** O "Descriptor" da aplicação. Arquivo de configuração central (JSON) que define o modelo de dados, as rotas de navegação, as bibliotecas carregadas e as configurações gerais do Fiori Elements.  
* **UI5 Tooling (ui5.yaml):** Ferramenta de linha de comando e configuração que gerencia o ciclo de vida do projeto (build, serve), dependências e proxies para conexão com o backend durante o desenvolvimento local.

#### **Comparativo: Onde configurar?**

| Tipo de Configuração | Onde fazer? | Ferramenta | Motivo |
| :---- | :---- | :---- | :---- |
| **Label do Campo** | Backend | ADT (Data Element) | Reutilização global e tradução centralizada. |
| **Posição da Coluna** | Backend | ADT (Metadata Extension) | Padrão para todos os consumidores do serviço. |
| **Ocultar Coluna (Específico)** | Frontend | Page Map (Local Annotation) | Regra específica deste aplicativo. |
| **Tema do App (Escuro/Claro)** | Frontend | Launchpad (User Settings) | Preferência do usuário, não do desenvolvedor. |
| **Botão com Javascript Custom** | Frontend | VS Code / BAS (Guided Dev) | Lógica de cliente que não existe no OData. |

### **📝 Quiz de Fixação**

Q1: Qual é a função primordial do "Application Generator" nas SAP Fiori Tools?  
R: Ele automatiza a criação da estrutura de pastas e arquivos de configuração (manifest.json, ui5.yaml) de um projeto Fiori, conectando-o corretamente a um serviço OData backend, poupando o desenvolvedor de configurações manuais complexas e propensas a erros.  
Q2: Se eu configurar uma coluna de tabela usando o "Page Map" do Fiori Tools (gerando uma anotação local), essa configuração afetará outros aplicativos que usam o mesmo serviço OData?  
R: Não. Configurações feitas no Page Map geram anotações locais (armazenadas dentro do projeto webapp do aplicativo específico). Elas não alteram o serviço OData no backend, portanto, outros aplicativos que consomem o mesmo serviço não verão essa alteração.  
Q3: O que é o arquivo manifest.json e por que ele é considerado o "coração" da aplicação Fiori?  
R: É o arquivo descritor da aplicação. Ele centraliza todas as configurações globais, incluindo a definição dos modelos de dados (OData), o roteamento entre páginas (navegação), as dependências de bibliotecas SAPUI5 e as configurações específicas dos componentes Fiori Elements. Sem ele, o aplicativo não sabe como iniciar ou se comportar.