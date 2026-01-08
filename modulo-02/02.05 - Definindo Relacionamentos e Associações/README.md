# **Módulo 02: Modelagem de Dados com ABAP Dictionary e CDS**

## **Aula 05: Definindo Relacionamentos e Associações**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Compreender profundamente a diferença arquitetural e de performance entre **JOINs** (SQL Clássico / Eager Loading) e **Associações** (CDS / Lazy Loading).  
2. Definir associações complexas utilizando a sintaxe association to ..., incluindo o uso de condições ON e parâmetros.  
3. Interpretar e configurar a **Cardinalidade** (\[0..1\], \[1..\*\], \[1..1\]), entendendo como ela influencia o otimizador do banco de dados e a geração de JOINS físicos.  
4. Expor associações na lista de projeção para habilitar **Propriedades de Navegação** em serviços OData, permitindo que aplicações Fiori naveguem entre entidades (Drill-down).

### **1\. O Conceito de Associação ("Join on Demand")**

A introdução das **Associações** no ABAP CDS representa uma mudança de paradigma na forma como pensamos sobre relacionamentos de dados.

* **A Abordagem Clássica (JOIN):** Em Views SQL tradicionais (SE11), quando definimos um LEFT OUTER JOIN entre "Pedido" e "Cliente", o banco de dados executa essa junção fisicamente **toda vez** que a view é lida. Se o seu relatório precisa apenas do ID do Pedido e do Valor Total, o banco desperdiça recursos buscando o Endereço do Cliente, mesmo que essa informação nunca seja exibida. Isso é chamado de *Eager Loading* (Carregamento Ansioso).  
* **A Abordagem Moderna (Associação):** No ABAP CDS, uma Associação é uma definição lógica de um relacionamento, não uma instrução de execução imediata. Ela funciona como um "Join sob demanda" (*Lazy Join*).  
  * Se você selecionar apenas campos da tabela primária, a associação permanece adormecida. O JOIN físico não acontece.  
  * O JOIN físico só é executado no momento em que um campo da entidade associada é explicitamente solicitado na consulta.  
  * **Benefício:** Isso economiza memória e CPU drasticamente no SAP HANA, permitindo criar views ricas com dezenas de associações sem penalizar a performance de consultas simples.

### **2\. Sintaxe da Associação**

A definição de uma associação ocorre logo após a cláusula FROM e antes da abertura da lista de campos {. Ela conecta a entidade fonte (a própria view) a uma entidade destino.

association \[cardinalidade\] to Entidade\_Destino as \_Alias  
    on $projection.MeuCampo \= \_Alias.CampoDeles

#### **Detalhes da Sintaxe:**

* **Convenção de Nome (\_):** É uma forte recomendação da SAP (e padrão de mercado) iniciar o alias de associações com um underscore (ex: \_Customer, \_Currency). Isso permite distinguir visualmente o que é uma coluna física de um relacionamento navegável.  
* **$projection:** Esta palavra-chave refere-se a um campo (Alias) definido na lista de seleção da própria view. Usar $projection em vez do nome do campo da tabela base é preferível pois torna a associação resiliente a mudanças na tabela física, desde que o alias da view se mantenha.  
* **Associações Filtradas:** Você também pode adicionar filtros fixos na condição ON. Exemplo: on $projection.OrgVendas \= \_Config.OrgVendas and \_Config.Status \= 'Ativo'.

### **3\. Entendendo a Cardinalidade**

A cardinalidade é uma instrução para o otimizador do banco de dados. Ela informa quantos registros podem existir na tabela de destino para cada registro da tabela de origem. A sintaxe é \[min..max\].

* **\[0..1\] (Zero ou Um):** Relacionamento "Para Um" opcional. Exemplo: Uma Viagem *pode* ter uma Agência. Se não tiver, retorna nulo. Comporta-se como um LEFT OUTER JOIN quando executado. Este é o padrão se a cardinalidade for omitida.  
* **\[1..1\] (Exatamente Um):** Relacionamento "Para Um" obrigatório. Exemplo: Um Item de Pedido *deve* ter um Cabeçalho. Isso dá liberdade ao banco para usar INNER JOIN se for mais performático, pois a existência é garantida.  
* **\[0..\*\] ou \[\*\] (Zero ou Muitos):** Relacionamento "Para Muitos". Exemplo: Uma Viagem tem *muitas* Reservas. Isso altera a granularidade dos dados se campos da associação forem selecionados (pode duplicar linhas da origem).

*Nota Técnica:* Definir a cardinalidade correta não é apenas documentação; ajuda o SAP HANA a escolher o melhor plano de execução SQL.

### **4\. Exemplo Prático: Enriquecendo a View de Viagens**

Vamos atualizar nossa Z\_I\_TRAVEL para conectar com as tabelas padrão de Agência, Cliente e Moeda. Observe como as associações enriquecem o modelo sem poluir a lista de dados principal.

@AccessControl.authorizationCheck: \#NOT\_REQUIRED  
@EndUserText.label: 'Interface View de Viagens'  
define view entity Z\_I\_TRAVEL  
  as select from zrap\_travel as Travel

  /\* Definição das Associações (Relacionamentos Lógicos) \*/  
    
  /\* \[0..1\] Uma viagem pode ou não ter uma agência definida no momento \*/  
  association \[0..1\] to /DMO/I\_Agency   as \_Agency     
    on $projection.AgencyID \= \_Agency.AgencyID  
      
  /\* \[0..1\] O cliente é opcional no rascunho, mas obrigatório depois \*/  
  association \[0..1\] to /DMO/I\_Customer as \_Customer   
    on $projection.CustomerID \= \_Customer.CustomerID  
      
  /\* \[0..1\] A moeda deve existir na tabela de moedas \*/  
  association \[0..1\] to I\_Currency      as \_Currency   
    on $projection.CurrencyCode \= \_Currency.Currency  
{  
  key travel\_uuid           as TravelUUID,  
      travel\_id             as TravelID,  
      agency\_id             as AgencyID,  
      customer\_id           as CustomerID,  
      begin\_date            as BeginDate,  
      end\_date              as EndDate,  
        
      @Semantics.amount.currencyCode: 'CurrencyCode'  
      total\_price           as TotalPrice,  
        
      currency\_code         as CurrencyCode,  
      description           as Description,  
      overall\_status        as OverallStatus,

      /\* \--- Exposição das Associações (Publicação) \--- \*/  
      /\* Ao incluir o alias da associação aqui, transformamos \*/  
      /\* o relacionamento "privado" em uma "Navegação Pública". \*/  
        
      \_Agency,  
      \_Customer,  
      \_Currency  
}

### **5\. Exposição de Associações: O Poder da Navegação**

Definir a associação (association to...) permite usá-la dentro da própria view (ex: para buscar o nome do cliente: \_Customer.LastName as CustomerName).

Porém, para que consumidores externos (como um App Fiori ou outra CDS View) possam usar esse relacionamento, precisamos **Expô-lo**. Isso é feito adicionando o alias (\_Customer) na lista de campos { }.

**Por que expor?**

1. **Reutilização:** Outra view que leia Z\_I\_TRAVEL pode acessar os campos do cliente sem precisar refazer o join: select from Z\_I\_TRAVEL { \_Customer.EmailAddress }.  
2. **OData Navigation:** No mundo OData, uma associação exposta vira uma **Navigation Property**. Isso permite que o Fiori Elements navegue de uma tela para outra automaticamente.  
   * *URL OData:* .../Travels('001')/to\_Customer  
   * Isso habilita o usuário a clicar no ID do Cliente na tela de Viagem e abrir um pop-over com os dados do contato do cliente, tudo gerenciado pelo framework através da associação.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Association (Associação):** Um relacionamento semântico entre duas entidades CDS. Diferente de um JOIN SQL, a associação só é materializada em tempo de execução se os dados da entidade destino forem solicitados ("Lazy Loading").  
* **Cardinality (Cardinalidade):** Expressão \[min..max\] que define a regra numérica do relacionamento (1:1, 1:N, N:M). É crucial para o otimizador do banco de dados decidir a estratégia de execução da query e prevenir duplicação indesejada de linhas.  
* **Navigation Property:** Conceito do protocolo OData. Quando uma associação CDS é exposta na lista de projeção, ela se torna uma propriedade navegável no serviço, permitindo acessar dados hierárquicos ou relacionados via URL (ex: .../Orders(1)/Items).  
* **\_Alias (Underscore):** Convenção de nomenclatura padrão da SAP para associações em CDS Views. O uso do prefixo \_ ajuda desenvolvedores a diferenciar visualmente campos físicos de colunas de relacionamentos navegáveis no código.  
* **$projection:** Palavra-chave usada na condição ON de uma associação para referenciar o alias de um campo definido na lista de seleção da view atual, em vez do nome técnico do campo na tabela base.

#### **Comparativo: Join vs Association**

| Característica | JOIN (SQL Clássico) | ASSOCIATION (CDS) |
| ----- | ----- | ----- |
| **Execução** | Eager: Sempre executa a junção de tabelas, independente do uso. | Lazy: Só executa a junção se campos do destino forem explicitamente solicitados. |
| **Sintaxe** | `LEFT OUTER JOIN Tabela ON ...` | `association [1..1] to Entidade ...` |
| **Reutilização** | Local para aquela query específica. | Pode ser exposta na projeção e reutilizada por consumidores (Views/Apps). |
| **Semântica** | Apenas técnica (colunas). | Define hierarquia, cardinalidade e relação de negócio (pai/filho). |

### **📝 Quiz de Fixação**

Q1: Qual é a principal vantagem de performance ao usar ASSOCIATION em vez de JOIN em uma CDS View complexa?  
R: A associação opera sob demanda ("Join-on-demand"). Se a query que consome a view não solicitar colunas da tabela associada, o banco de dados não realiza a junção física, economizando processamento (CPU) e memória, o que é vital em views com dezenas de relacionamentos.  
Q2: O que significa tecnicamente a cardinalidade \[0..\*\] em uma associação?  
R: Significa que para cada registro da tabela origem, podem existir zero, um ou muitos registros na tabela destino (Relacionamento "Um para Muitos"). O uso incorreto dessa cardinalidade ao acessar dados pode causar a duplicação de linhas na tabela de origem (explosão cartesiana).  
Q3: Para que uma aplicação Fiori (OData) possa navegar de uma entidade "Viagem" para os detalhes do "Cliente" associado (Drill-down), o que deve ser feito na CDS View?  
R: A associação \_Customer deve ser definida com a condição correta e, crucialmente, deve ser exposta (adicionada) na lista de seleção de campos da view (dentro das chaves {}). Sem a exposição, a associação é apenas interna e não visível para o serviço OData.