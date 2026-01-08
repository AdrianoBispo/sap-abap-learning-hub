# **Módulo 03: Introdução à Criação de um Aplicativo SAP Fiori Elements**

## **Aula 07: Configurando Tabelas (Cores e Ações)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Implementar **Criticality** (Cores Semânticas) para destacar colunas de status, compreendendo como mapear valores numéricos do backend para feedback visual no frontend.  
2. Adicionar **Botões de Ação** na tabela (DataFieldForAction), distinguindo entre ações globais (Toolbar) e ações específicas de linha (Inline).  
3. Gerenciar a visibilidade de botões padrão (**Create**, **Delete**, **Update**) via anotações de UI, independentemente das capacidades do backend.  
4. Configurar a **Responsividade** da tabela usando a propriedade importance, garantindo uma experiência de usuário fluida em dispositivos móveis e desktops.

### **1\. Dando Cor aos Dados: Criticality**

Em aplicações corporativas, os usuários frequentemente precisam processar grandes listas de dados rapidamente. Uma tabela monótona cheia de texto preto e branco dificulta a identificação de problemas. O SAP Fiori utiliza o conceito de **Criticality** (Criticidade) para adicionar uma camada semântica visual aos dados.

Não escolhemos cores arbitrárias (como "Azul \#0000FF"); usamos **Cores Semânticas** que transmitem significado universal:

* **3 (Verde \- Positive):** Sucesso, Aprovado, Concluído, Dentro do Prazo.  
* **2 (Amarelo \- Critical):** Aviso, Em Atenção, Pendente, Quase Vencendo.  
* **1 (Vermelho \- Negative):** Erro, Rejeitado, Atrasado, Bloqueado.  
* **0 (Cinza \- Neutral):** Informativo, Inativo, Rascunho, Sem Status.

#### **Passo 1: O Campo Calculado (Backend)**

A lógica de decisão ("Este registro é crítico?") deve residir no Backend (CDS View), não no Frontend. Criamos um campo calculado, geralmente nomeado como StatusCriticality, que retorna os inteiros 0, 1, 2 ou 3\.

/\* Exemplo na CDS View \*/  
case overall\_status  
  when 'A' then 3  \-- Aceito \= Verde  
  when 'X' then 1  \-- Cancelado \= Vermelho  
  when 'O' then 2  \-- Aberto \= Amarelo  
  else 0           \-- Outros \= Cinza  
end as StatusCriticality

#### **Passo 2: A Anotação (Metadata Extension)**

Na Metadata Extension, vinculamos a coluna visual ao campo de cálculo técnico.

/\* Na coluna OverallStatus \*/  
@UI.lineItem: \[   
  {   
    position: 50,   
    label: 'Status',  
      
    /\* Vínculo Mágico: O Fiori lê o valor desta coluna para pintar a linha \*/  
    criticality: 'StatusCriticality',   
      
    /\* Opcional: Adiciona ícone além da cor (Check, X, Exclamação) \*/  
    /\* Recomendado para Acessibilidade (Daltônicos) \*/  
    criticalityRepresentation: \#WITH\_ICON   
  }   
\]  
OverallStatus;

**Resultado:** Se o status for 'A' (Aceito), o campo StatusCriticality será 3\. O Fiori pintará o texto "Aceito" de verde e adicionará um ícone de "check" ao lado.

### **2\. Botões de Ação na Tabela**

Uma tabela moderna não serve apenas para leitura; ela é um ponto de partida para ações. No Fiori Elements, as ações podem aparecer em dois lugares principais: na **Barra de Ferramentas (Toolbar)** ou dentro de cada **Linha (Inline)**.

#### **A. Ações Padrão (C.R.U.D.)**

O framework gera automaticamente os botões **Create** (Criar) e **Delete** (Excluir) baseando-se nas capacidades do seu Behavior Definition (BDEF).

* Se o BDEF diz create;, o botão "Criar" aparece.  
* Se o BDEF diz delete;, o botão "Excluir" aparece (ativado ao selecionar linhas).

Controle via UI: Às vezes, o backend permite deletar (tecnicamente), mas queremos esconder o botão na tela principal por regra de negócio ou usabilidade.  
Usamos anotações de cabeçalho:

* @UI.createHidden: true  
* @UI.deleteHidden: true  
* @UI.updateHidden: true (Esconde o botão "Editar" na Object Page).

#### **B. Ações Customizadas (Botões de Negócio)**

Para ações específicas como "Aceitar Viagem" ou "Copiar", usamos a anotação \#FOR\_ACTION.

**Tipos de Posicionamento:**

1. **Toolbar Action:** Botão no topo da tabela. Ideal para ações em massa ou que afetam múltiplos registros selecionados. (Padrão se não especificado o contrário).  
2. **Inline Action:** Botão pequeno dentro da própria linha. Ideal para ações rápidas em um único registro. Configurado adicionando a anotação ao campo relevante ou usando invocationGrouping: \#ISOLATED (dependendo da versão).

@UI.lineItem: \[  
  /\* Coluna de Dados Normal \*/  
  { position: 10, label: 'Viagem' },

  /\* Ação na Toolbar: Aceitar \*/  
  /\* Requer seleção de uma ou mais linhas \*/  
  {   
    type: \#FOR\_ACTION,   
    dataAction: 'acceptTravel',   /\* Nome exato da ação no BDEF \*/  
    label: 'Aceitar Viagem',  
    position: 15   
  },  
    
  /\* Ação Inline: Rejeitar \*/  
  /\* Aparece como um link ou botão na linha se configurado como Inline \*/  
  {   
    type: \#FOR\_ACTION,   
    dataAction: 'rejectTravel',   
    label: 'Rejeitar',  
    position: 16   
  }  
\]  
TravelID;

### **3\. Responsividade e Importância**

O Fiori Elements segue a filosofia "Mobile First". Mas o que acontece quando uma tabela com 20 colunas é aberta em um iPhone?

O framework usa a propriedade importance para decidir o destino de cada coluna:

* **\#HIGH:** A coluna é sagrada. Ela sempre permanece visível, não importa quão pequena seja a tela. Use para chaves (ID) e status críticos.  
* **\#MEDIUM:** A coluna é importante, mas secundária. Em tablets, ela aparece. Em celulares, ela é removida da tabela e movida para uma área de detalhes expansível (Pop-in).  
* **\#LOW:** A coluna é detalhe. Só aparece em monitores Desktop grandes. Em telas menores, ela vai para o Pop-in ou é ocultada.

**O Conceito de "Pop-in":** Quando uma coluna é escondida por falta de espaço, o Fiori não apaga os dados. Ele cria uma seta na linha que, ao ser clicada, expande a linha para baixo mostrando os campos ocultos em formato de lista (Label: Valor).

/\* Descrição é longa, então só mostramos em telas grandes \*/  
@UI.lineItem: \[ { position: 30, importance: \#LOW } \]  
Description;

### **4\. Exemplo Prático Completo**

Vamos configurar uma tabela de viagens rica, com cores, ícones e ações estratégicas.

annotate view Z\_C\_TRAVEL with  
{  
  /\* \--- AÇÕES GLOBAIS E COLUNA CHAVE \--- \*/  
  @UI.lineItem: \[   
    /\* Botão de Ação Global (Toolbar): Copiar \*/  
    /\* Posição baixa (5) coloca o botão à esquerda na toolbar \*/  
    { type: \#FOR\_ACTION, dataAction: 'copyTravel', label: 'Copiar Viagem', position: 5 },  
      
    /\* Coluna ID: Sempre visível (\#HIGH) \*/  
    { position: 10, importance: \#HIGH }   
  \]  
  TravelID;

  /\* \--- DADOS SECUNDÁRIOS \--- \*/  
  @UI.lineItem: \[ { position: 20, importance: \#HIGH } \]  
  AgencyID;

  @UI.lineItem: \[ { position: 30, importance: \#MEDIUM } \]  
  CustomerID;

  @UI.lineItem: \[ { position: 40, importance: \#MEDIUM } \]  
  BeginDate;

  /\* \--- STATUS COLORIDO \--- \*/  
  /\* Criticality aponta para o campo calculado StatusCriticality (0-3) \*/  
  /\* Representation \#WITH\_ICON adiciona ícones visuais \*/  
  @UI.lineItem: \[   
    {   
      position: 50,   
      importance: \#HIGH,  
      criticality: 'StatusCriticality',  
      criticalityRepresentation: \#WITH\_ICON,  
      label: 'Status Aprovação'  
    }   
  \]  
  OverallStatus;  
    
  /\* \--- AÇÃO INLINE \--- \*/  
  /\* Este botão aparecerá "atrelado" à coluna BookingFee \*/  
  /\* Ideal para ações contextuais rápidas \*/  
  @UI.lineItem: \[   
    { type: \#FOR\_ACTION, dataAction: 'deductFee', label: 'Deduzir Taxa', position: 60 }   
  \]  
  BookingFee;   
}

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Criticality (Criticidade):** Recurso de UI que altera a cor semântica de um campo (texto ou ícone) baseando-se em um valor numérico (0=Neutro/Cinza, 1=Erro/Vermelho, 2=Aviso/Amarelo, 3=Sucesso/Verde). Essencial para dashboards e listas de exceção.  
* **\#FOR\_ACTION:** Tipo de item de linha (type) usado na anotação @UI.lineItem para renderizar um botão que dispara uma ação BDEF (Behavior Definition). O framework cuida da chamada OData (POST) automaticamente.  
* **DataAction:** Propriedade da anotação \#FOR\_ACTION que define o nome técnico da ação (como definido no Behavior Definition) que será executada. Deve corresponder exatamente ao nome no backend.  
* **Importance:** Propriedade de responsividade que define a prioridade de exibição de uma coluna (\#HIGH, \#MEDIUM, \#LOW). Determina se a coluna permanece na tabela ou se move para a área de detalhes (Pop-in) em telas menores.  
* **CriticalityRepresentation:** Define se a criticidade será mostrada apenas com cor no texto ou se incluirá um ícone (\#WITH\_ICON), melhorando a acessibilidade e leitura rápida.  
* **Inline Action:** Uma ação configurada para aparecer dentro da linha da tabela, ao invés da barra de ferramentas superior, facilitando o acesso rápido sem necessidade de seleção prévia.

#### **Tabela de Cores (Criticality)**

| Valor Backend | Significado | Cor Fiori | Ícone Típico (com \#WITH\_ICON) |
| :---- | :---- | :---- | :---- |
| **0** | Neutro | Cinza | Nenhum |
| **1** | Negativo | Vermelho | X / Erro |
| **2** | Crítico | Laranja/Amarelo | \! / Triângulo de Alerta |
| **3** | Positivo | Verde | V / Check de Sucesso |

### **📝 Quiz de Fixação**

Q1: Onde deve ser calculada a lógica que define se um status é vermelho (1) ou verde (3)?  
R: Preferencialmente na CDS View (usando a expressão CASE), criando um campo dedicado (ex: StatusCriticality). A Metadata Extension apenas aponta para esse campo. Isso mantém a lógica de negócio no backend (Code Pushdown) e deixa o frontend apenas com a renderização.  
Q2: Se eu definir uma coluna com importance: \#LOW, o que acontece visualmente quando abro o aplicativo em um celular (tela estreita)?  
R: A coluna será ocultada da grade principal da tabela para economizar espaço horizontal. Dependendo da configuração da tabela, os dados dessa coluna podem ficar acessíveis ao clicar na linha ou expandir um detalhe ("Pop-in"), mas não quebrarão o layout da tabela.  
Q3: Para adicionar um botão customizado na tabela (ex: "Aprovar"), qual anotação devo usar e qual pré-requisito é necessário no Backend?  
R: Devo adicionar um item na anotação @UI.lineItem com a propriedade type: \#FOR\_ACTION e apontar o dataAction para o nome da ação. O pré-requisito é que essa Ação (Action) esteja definida e implementada no Behavior Definition (BDEF) da entidade.