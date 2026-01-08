# **Módulo 02: Modelagem de Dados com ABAP Dictionary e CDS**

## **Aula 04: Definindo CDS Views Básicas (Interface Views)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Escrever uma **CDS View Entity** utilizando a sintaxe moderna e estrita, compreendendo as melhorias de performance e arquitetura em relação às views clássicas.  
2. Aplicar rigorosamente as convenções de nomenclatura do **Virtual Data Model (VDM)**, diferenciando **Interface Views (I\_)** de **Consumption Views (C\_)** e entendendo o propósito de reutilização de cada uma.  
3. Utilizar **Aliases** estrategicamente para converter nomes técnicos legados (ex: MATNR) para nomes semânticos em **CamelCase** (ex: MaterialID), facilitando o consumo por interfaces web (UI5/Fiori).  
4. Entender a diferença técnica crítica entre DEFINE VIEW (Obsoleto, gera artefatos SE11) e DEFINE VIEW ENTITY (Novo Padrão, gerenciado pelo Kernel ABAP).

### **1\. A Nova Sintaxe: View Entity vs. CDS View Clássica**

Nos primeiros anos da tecnologia CDS (Core Data Services), utilizávamos o comando DEFINE VIEW. Embora revolucionário, ele carregava um débito técnico: a duplicidade de artefatos.

#### **O Problema da Abordagem Antiga (DEFINE VIEW)**

Ao ativar uma CDS View clássica, o sistema criava dois objetos:

1. **Entidade CDS:** O objeto rico em semântica, visível no Eclipse.  
2. View de Banco de Dados (DDIC View): Uma view clássica na transação SE11 (com limite de 16 caracteres no nome).  
   Consequência: Isso gerava problemas de namespace, ativações lentas e limitações técnicas, pois a view precisava ser compatível com as regras antigas do Dicionário ABAP.

#### **A Solução Moderna (DEFINE VIEW ENTITY)**

Desde o ABAP 7.55 (e padrão obrigatório no ABAP Cloud), usamos **DEFINE VIEW ENTITY**.

* **Sem Artefato SE11:** Não cria nenhuma view na SE11. A entidade existe apenas no nível do CDS e é gerenciada diretamente pelo Kernel ABAP e pelo banco de dados HANA.  
* **Verificação Estrita:** O compilador é mais rigoroso. Tipos de dados devem coincidir perfeitamente, e certas ambiguidades do SQL antigo não são toleradas, resultando em código mais limpo e seguro.  
* **Performance:** A ativação é muito mais rápida, e o plano de execução no banco de dados pode ser otimizado de forma mais eficiente pelo otimizador do HANA.

### **2\. Estrutura de uma CDS View**

Uma View CDS é um artefato de código fonte (DDL \- Data Definition Language) composto por três partes principais:

#### **A. Anotações (Header Annotations)**

Configurações técnicas que precedem a definição. Começam com @.

* @AccessControl.authorizationCheck: Define se a view terá controle de acesso automático (DCL). Para Interface Views básicas, muitas vezes usamos \#NOT\_REQUIRED ou \#CHECK.  
* @EndUserText.label: A descrição da view. Obrigatória em View Entities.

#### **B. Definição e Fonte de Dados**

Onde declaramos o nome da entidade e de onde ela busca dados.  
define view entity NomeDaView as select from FonteDeDados as Alias

#### **C. Lista de Seleção (Projection List)**

O "miolo" da view, dentro das chaves { }. Aqui selecionamos campos, criamos cálculos, expomos associações e aplicamos anotações de campo.

#### **Convenção de Nomes e VDM (Virtual Data Model)**

O VDM organiza as milhares de views do S/4HANA.

* **Interface Views (I\_):** A base da pirâmide. Devem ser agnósticas de UI, reutilizáveis e estáveis. Espelham os dados do negócio. Ex: Z\_I\_Travel.  
* **Consumption Views (C\_):** O topo da pirâmide. Específicas para um aplicativo ou relatório. Consomem as Interface Views. Ex: Z\_C\_Travel\_Analytics.

### **3\. Exemplo Prático: Criando a Interface de Viagens**

Vamos criar a view Z\_I\_TRAVEL. O objetivo é ler a tabela física ZRAP\_TRAVEL e transformar seus campos técnicos em uma interface de negócio limpa.

**Atenção ao CamelCase:** Note como usamos as TravelUUID em vez de deixar travel\_uuid. Interfaces modernas (Fiori, React, APIs REST) padronizam o uso de CamelCase. Se mandarmos TRAVEL\_UUID, o frontend JavaScript terá que lidar com nomes fora do padrão. O CDS resolve isso na fonte.

@AccessControl.authorizationCheck: \#NOT\_REQUIRED  
@EndUserText.label: 'Interface View para Viagens'  
@Metadata.ignorePropagatedAnnotations: true 

define view entity Z\_I\_TRAVEL  
  as select from zrap\_travel as Travel  
{  
  /\* Chaves: Essenciais para o funcionamento do OData e navegação \*/  
  key travel\_uuid           as TravelUUID,

  /\* Campos de Identificação de Negócio \*/  
  travel\_id             as TravelID,  
  agency\_id             as AgencyID,  
  customer\_id           as CustomerID,  
    
  /\* Datas \*/  
  begin\_date            as BeginDate,  
  end\_date              as EndDate,  
    
  /\* Valores Monetários: A ligação Semântica é feita aqui ou na tabela \*/  
  @Semantics.amount.currencyCode: 'CurrencyCode'  
  booking\_fee           as BookingFee,  
    
  @Semantics.amount.currencyCode: 'CurrencyCode'  
  total\_price           as TotalPrice,  
    
  /\* Moeda e Descrições \*/  
  currency\_code         as CurrencyCode,  
  description           as Description,  
    
  /\* Status do Processo \*/  
  overall\_status        as OverallStatus,

  /\* \--- Campos de Auditoria (Admin Data) \--- \*/  
  /\* Estas anotações permitem que o RAP preencha os dados automaticamente \*/  
  @Semantics.user.createdBy: true  
  created\_by            as CreatedBy,  
    
  @Semantics.systemDateTime.createdAt: true  
  created\_at            as CreatedAt,  
    
  @Semantics.user.lastChangedBy: true  
  last\_changed\_by       as LastChangedBy,  
    
  @Semantics.systemDateTime.lastChangedAt: true  
  last\_changed\_at       as LastChangedAt

}

### **4\. Anotações Semânticas: O Segredo da Automação**

No código acima, as anotações @Semantics não são decorativas; elas alteram o comportamento do sistema.

#### **Semântica de Moeda e Quantidade**

* @Semantics.amount.currencyCode: 'CurrencyCode'  
  * **O que faz:** Vincula o campo de valor (TotalPrice) ao campo de moeda (CurrencyCode).  
  * **Impacto na UI:** O Fiori Elements sabe que não deve apenas mostrar "100", mas sim "100,00 EUR" ou "100 JPY" (sem decimais), aplicando a formatação correta baseada na moeda.

#### **Semântica de Auditoria (RAP Managed)**

* @Semantics.user.createdBy: true  
  * **O que faz:** Marca o campo como "Usuário de Criação".  
  * **Impacto no Backend:** Em um cenário RAP Managed (que veremos adiante), o framework identifica essa anotação e preenche automaticamente o campo com o usuário logado (sy-uname) no momento do INSERT. O desenvolvedor não precisa escrever uma linha de código para isso.

### **5\. Code Pushdown: Cálculos na View**

Uma das maiores vantagens do CDS é realizar cálculos linha a linha diretamente no banco de dados, evitando loops no ABAP.

#### **Lógica Condicional (CASE)**

Categorizar dados na fonte é muito mais eficiente.

case   
  when total\_price \> 1000 then 'High Value'   
  when total\_price \> 500  then 'Medium Value'  
  else 'Low Value'   
end as PriceCategory

#### **Operações de String e Data**

Funções embutidas permitem tratar dados brutos.

/\* Concatenação \*/  
concat\_with\_space(first\_name, last\_name, 1\) as FullName

/\* Cálculo de Dias \*/  
dats\_days\_between(begin\_date, end\_date) as DurationDays

/\* Conversão de Tipo (Casting) \*/  
cast(total\_price as abap.fltp) as PriceFloat

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **CDS View Entity:** A evolução da CDS View. Uma entidade de projeção SQL gerenciada inteiramente pelo kernel ABAP, que não gera artefatos correspondentes no Dicionário ABAP clássico (SE11). Oferece melhor performance e validação de sintaxe.  
* **Alias (Apelido):** Nome alternativo dado a um campo na lista de seleção (usando as NovoNome). No VDM, usamos Aliases para converter nomes técnicos do banco (MATNR) para nomes legíveis e padronizados (MaterialID).  
* **CamelCase:** Estilo de escrita onde as palavras são unidas sem espaços e cada palavra subsequente começa com maiúscula (ex: FlightDate). É o padrão para nomes de campos em CDS Views modernas para facilitar o consumo por JavaScript/UI5.  
* **Key (Chave):** Palavra-chave obrigatória em CDS Views. Define quais campos identificam unicamente um registro. Sem chaves definidas corretamente, o framework OData não consegue realizar operações de leitura (Read) ou navegação em registros individuais.  
* **@Semantics:** Família de anotações que descrevem o significado do dado (ex: isto é um e-mail, isto é uma moeda, isto é um usuário de criação). Essencial para automações do framework RAP e renderização correta no Fiori.

#### **Comparativo: DDIC View vs CDS View Entity**

| Característica | DEFINE VIEW (Antigo) | DEFINE VIEW ENTITY (Novo) |
| ----- | ----- | ----- |
| **Artefato SE11** | Cria uma View de Banco de Dados | Nenhum (Só existe no CDS) |
| **Performance** | Mais lenta na ativação | Mais rápida e otimizada |
| **Cálculos** | Limitados em alguns cenários | Suporte total a expressões SQL |
| **Recomendação** | Apenas para legado (7.40) | Sempre usar no S/4HANA |

### **📝 Quiz de Fixação**

Q1: Qual é a principal diferença técnica entre usar DEFINE VIEW e DEFINE VIEW ENTITY?  
R: DEFINE VIEW cria uma View CDS e uma View de Banco de Dados clássica (SE11) duplicada, o que pode causar conflitos de nome e overhead. DEFINE VIEW ENTITY cria apenas a entidade CDS, sendo processada inteiramente pelo runtime do CDS e otimizada para o HANA, sem gerar artefatos desnecessários no dicionário.  
Q2: Por que renomeamos os campos usando Aliases (ex: travel\_id as TravelID) nas Interface Views?  
R: Para padronizar os nomes seguindo a convenção CamelCase. Isso torna o modelo de dados semanticamente mais rico e amigável para o desenvolvimento de interfaces web (UI5/Fiori) e APIs OData, que naturalmente utilizam esse padrão de nomenclatura.  
Q3: O que acontece se eu esquecer de marcar um campo com a palavra-chave key na CDS View?  
R: A view funcionará sintaticamente para seleções em massa, mas poderá causar erros graves ao ser consumida por frameworks OData ou Fiori Elements. Esses frameworks precisam saber qual é o identificador único da linha para realizar operações de leitura de detalhe, edição ou navegação.  
Q4: Para que serve a anotação @Semantics.user.createdBy: true em um cenário RAP Managed?  
R: Ela instrui o framework RAP a preencher automaticamente esse campo com o ID do usuário logado durante a operação de criação (CREATE) do registro, eliminando a necessidade de implementação manual dessa lógica.