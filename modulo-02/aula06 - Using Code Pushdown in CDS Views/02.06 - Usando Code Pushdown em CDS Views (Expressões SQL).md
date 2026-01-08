# **Módulo 02: Modelagem de Dados com ABAP Dictionary e CDS**

## **Aula 06: Usando Code Pushdown em CDS Views (Expressões SQL)**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Aplicar lógica condicional complexa com **CASE Expressions** diretamente na CDS View, substituindo estruturas de controle ABAP (IF/ELSEIF) por lógica de banco de dados.  
2. Realizar cálculos aritméticos avançados e manipulação de datas utilizando funções nativas como dats\_days\_between, dats\_add\_days e operações matemáticas.  
3. Converter tipos de dados explicitamente utilizando a instrução **CAST**, garantindo a integridade dos dados em operações de cálculo e uniões.  
4. Utilizar **Variáveis de Sessão** ($session.system\_date, $session.user, $session.system\_language) para criar views dinâmicas que respondem ao contexto do usuário logado.

### **1\. O Poder das Expressões SQL e o Fim do Loop**

No desenvolvimento ABAP clássico, o banco de dados era visto apenas como um repositório passivo. O padrão era: "Selecionar todos os dados brutos, transferi-los para o servidor de aplicação, armazenar em tabelas internas e então iterar (LOOP) para realizar cálculos linha a linha".

O Problema (Data-to-Code):  
Essa abordagem gera um tráfego de rede imenso e subutiliza a capacidade de processamento paralelo do banco de dados. Se você precisa calcular o "Valor Total com Imposto" de 1 milhão de registros, trazer tudo para o ABAP consome memória e CPU desnecessariamente.  
A Solução (Code Pushdown com CDS):  
Com o Code Pushdown, transferimos essa lógica para a definição da View. O banco de dados (HANA) realiza os cálculos durante a leitura. O ABAP recebe apenas o resultado final processado.

* **Vantagem de Performance:** O cálculo ocorre onde os dados residem. O HANA pode paralelizar a operação em múltiplos núcleos.  
* **Vantagem de Manutenção:** A regra de negócio (ex: como calcular um status) fica centralizada na View, reutilizável por qualquer consumidor (Relatório, App Fiori, API), em vez de estar escondida dentro de um método ABAP específico.

### **2\. Lógica Condicional: CASE Expressions**

O comando CASE é o equivalente SQL para o IF...ELSEIF...ELSE do ABAP. Ele permite criar colunas calculadas cujo valor depende de condições lógicas em outros campos da linha.

#### **Sintaxe Básica e Avançada**

Podemos usar o CASE simples (comparando um campo) ou o CASE complexo (com múltiplas condições distintas).

**Exemplo: Traduzindo Códigos Técnicos para Textos de Negócio**

case overall\_status  
  when 'O' then 'Em Aberto'  
  when 'A' then 'Aceito'  
  when 'X' then 'Cancelado'  
  else 'Desconhecido' \-- Fallback para valores inesperados  
end as StatusText

Exemplo: Criticality (Cores no Fiori)  
Um uso extremamente comum no RAP é definir a "Criticidade" de uma linha. O Fiori Elements entende valores numéricos (0=Neutro, 1=Negativo, 2=Crítico, 3=Positivo) para colorir textos e ícones.  
/\* Lógica de Negócio para Cores \*/  
case   
  when overall\_status \= 'X' then 1  \-- Vermelho (Erro)  
  when overall\_status \= 'O' and dats\_days\_between(created\_at, $session.system\_date) \> 30 then 2 \-- Amarelo (Atrasado)  
  when overall\_status \= 'A' then 3  \-- Verde (Sucesso)  
  else 0                            \-- Cinza (Neutro)  
end as StatusCriticality

*Dica:* Observe como combinamos uma comparação simples (=) com uma função de data (dats\_days\_between) dentro da condição WHEN. O CDS permite essa flexibilidade.

### **3\. Funções de Data, Aritmética e String**

O ABAP CDS oferece uma biblioteca rica de funções integradas que eliminam a necessidade de processamento posterior no ABAP.

#### **Aritmética**

Podemos realizar as quatro operações básicas e usar funções matemáticas. O CDS lida automaticamente com nulos (se um operando for NULL, o resultado é NULL).

* **Operadores:** \+, \-, \*, / (Divisão exata, retorna Floating Point).  
* **Funções:** div(a, b) (Divisão inteira), mod(a, b) (Resto), abs(x) (Valor absoluto), ceil(x) (Arredondar para cima), floor(x) (Arredondar para baixo).

/\* Exemplo: Calcular valor com margem de segurança \*/  
ceil( ( total\_price \* 1.1 ) ) as PriceWithMargin

#### **Datas e Timestamps**

Manipular datas no banco é essencial para relatórios de envelhecimento (Aging) ou duração.

* **dats\_days\_between( data\_inicio, data\_fim ):** Retorna um inteiro com a diferença em dias.  
* **dats\_add\_days( data, dias ):** Adiciona (ou subtrai) dias a uma data.  
* **tstmp\_current\_utctimestamp():** Retorna o timestamp atual (Data+Hora) em UTC.

/\* Calcular Data de Vencimento (30 dias após criação) \*/  
dats\_add\_days( begin\_date, 30 ) as DueDate

#### **Variáveis de Sessão ($session)**

No ABAP SQL clássico, usávamos sy-datum ou sy-uname. No DDL (definição da view), não podemos acessar variáveis do programa ABAP diretamente. Usamos as variáveis de sessão providas pelo banco de dados:

* **$session.user:** O ID do usuário logado. Útil para filtros "Meus Documentos".  
* **$session.client:** O mandante atual (MANDT). Geralmente tratado automaticamente.  
* **$session.system\_language:** O idioma de logon. Usado para selecionar textos.  
* **$session.system\_date:** A data atual do servidor.

/\* Flag: É minha viagem? \*/  
case   
  when created\_by \= $session.user then 'X'   
  else ''   
end as IsMyTravel

### **4\. Conversão de Tipos: CAST**

O comando CAST é vital quando precisamos alinhar tipos de dados. O ABAP e o SQL são estritos quanto à tipagem (Type Safety).

**Cenários Comuns:**

1. **Cálculos Aritméticos:** Às vezes, multiplicar um Inteiro por um Decimal pode exigir conversão explícita para evitar overflow ou perda de precisão.  
2. **Concatenação:** Não é possível concatenar um número com uma string diretamente. É preciso converter o número para char/string primeiro.  
3. **Uniões (UNION):** Ao unir duas tabelas, as colunas correspondentes devem ter tipos compatíveis.

**Sintaxe:** cast( expressão as TipoDestino )

Tipos de destino comuns no CDS:

* abap.char(len)  
* abap.numc(len)  
* abap.int4  
* abap.dec(len, decimals)  
* abap.fltp (Floating Point \- recomendado para cálculos complexos)

/\* Convertendo Preço para Float para cálculo preciso \*/  
cast( total\_price as abap.fltp ) as PriceFloat

/\* Convertendo para Char para uso em string \*/  
cast( travel\_id as abap.char(8) ) as TravelIdChar

### **5\. Exemplo Prático: Enriquecendo a View de Viagens**

Vamos aplicar todos esses conceitos na nossa Interface View Z\_I\_TRAVEL\_COMPUTED. Imagine que precisamos de campos calculados para suportar uma interface Fiori rica em informações visuais.

@AccessControl.authorizationCheck: \#NOT\_REQUIRED  
@EndUserText.label: 'Viagens com Cálculos Avançados'  
define view entity Z\_I\_TRAVEL\_COMPUTED  
  as select from zrap\_travel  
{  
  key travel\_uuid,  
      travel\_id,  
      begin\_date,  
      end\_date,  
      total\_price,  
      booking\_fee,  
      currency\_code,  
      overall\_status,  
      created\_by,

      /\* \--- 1\. Aritmética e Conversão \--- \*/  
      /\* Calcula o Custo Total Real (Preço \+ Taxa) \*/  
      /\* O CAST garante que ambos sejam tratados como ponto flutuante para precisão \*/  
      @Semantics.amount.currencyCode: 'currency\_code'  
      cast( total\_price as abap.fltp ) \+ cast( booking\_fee as abap.fltp ) as GrandTotal,

      /\* \--- 2\. Funções de Data \--- \*/  
      /\* Calcula a duração da viagem em dias \*/  
      dats\_days\_between( begin\_date, end\_date ) as DurationDays,

      /\* Verifica quantos dias faltam para o início (ou se já passou) \*/  
      dats\_days\_between( $session.system\_date, begin\_date ) as DaysUntilStart,

      /\* \--- 3\. Lógica Condicional (Criticality) \--- \*/  
      /\* Define a cor do status para o Fiori (3=Verde, 2=Amarelo, 1=Vermelho) \*/  
      case overall\_status  
        when 'A' then 3  \-- Aceito (Verde)  
        when 'O' then 2  \-- Aberto (Amarelo)  
        when 'X' then 1  \-- Cancelado (Vermelho)  
        else 0           \-- Desconhecido (Cinza)  
      end as StatusCriticality,

      /\* \--- 4\. Variável de Sessão e Lógica Booleana \--- \*/  
      /\* Cria um indicador visual se a viagem já aconteceu (Histórico) \*/  
      case   
        when begin\_date \< $session.system\_date then 'X'  
        else ''  
      end as IsPastTravel,  
        
      /\* Verifica se o usuário atual é o dono do registro \*/  
      case   
        when created\_by \= $session.user then 'X'  
        else ''  
      end as IsCreatedByMe

}

### **6\. Agregações (SUM, MAX, MIN)**

Embora as Interface Views (como a acima) geralmente retornem uma linha para cada registro da tabela (granularidade 1:1), o CDS também é poderoso para criar views analíticas (OLAP).

Para isso, usamos funções de agregação e a cláusula GROUP BY.

* **Regra do GROUP BY:** Se você usar uma função de agregação (SUM, COUNT, AVG, MIN, MAX) em qualquer campo da lista de seleção, **todos** os outros campos que não são agregados devem obrigatoriamente constar na cláusula GROUP BY.

@EndUserText.label: 'Estatísticas de Viagem por Cliente'  
define view entity Z\_I\_TRAVEL\_STATS  
  as select from zrap\_travel  
{  
  /\* Chave do Agrupamento \*/  
  key customer\_id,  
        
      /\* Agregações \*/  
      count(\*) as NumberOfTravels,  
        
      @Semantics.amount.currencyCode: 'CurrencyCode'  
      sum( total\_price ) as TotalSpent,  
        
      avg( total\_price as abap.dec(15,2) ) as AverageTicketPrice,  
        
      /\* Campo de Controle para Semântica \*/  
      currency\_code as CurrencyCode  
}  
/\* Agrupamento Obrigatório \*/  
group by customer\_id, currency\_code

*Nota:* Neste exemplo, se um cliente tiver viagens em EUR e USD, ele aparecerá em duas linhas diferentes (uma para cada moeda), pois currency\_code faz parte do grupo.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico**

* **Expression (Expressão SQL):** Qualquer comando que gera um valor novo em tempo de execução, em vez de apenas ler um valor armazenado. Inclui cálculos, concatenações e lógica condicional.  
* **CAST (Type Casting):** Função SQL usada para converter explicitamente um valor de um tipo de dados para outro (ex: NUMC para INT). Essencial para evitar erros de tipo em cálculos aritméticos e uniões.  
* **Session Variable ($session):** Variáveis globais de contexto acessíveis dentro da CDS View que contêm informações do ambiente de execução atual do banco de dados/sessão ABAP. Exemplos: $session.system\_date (Data atual), $session.user (Usuário logado).  
* **Aggregations (Agregações):** Funções que condensam múltiplos registros em um único resultado resumo, como SUM (Soma), MIN (Mínimo), MAX (Máximo), AVG (Média) e COUNT (Contagem). Exigem o uso da cláusula GROUP BY.  
* **Criticality (Criticidade):** Conceito de UI do SAP Fiori onde valores numéricos (0-3) são mapeados para cores semânticas (Cinza, Vermelho, Amarelo, Verde) para indicar status ou severidade de forma visual.

#### **Code Pushdown na Prática: Comparativo**

| Tarefa | ABAP Clássico (Lento / Data-to-Code) | ABAP CDS (Rápido / Code-to-Data) |
| ----- | ----- | ----- |
| Status Text | Loop na tabela interna \+ IF/ELSE. | Coluna CASE na CDS View. |
| Duração | Loop \+ subtrair variáveis de data. | Função dats\_days\_between() na View. |
| Totalizar | Loop \+ COLLECT ou AT END OF. | Funções SUM() e GROUP BY na View. |
| Data Atual | Variável sy-datum no programa. | Variável $session.system\_date na View. |
| Filtro Usuário | SELECT ... WHERE user \= sy-uname. | WHERE user \= $session.user na View. |

### **📝 Quiz de Fixação**

Q1: Qual é a variável de sessão correta para filtrar ou comparar dados com a data atual do sistema dentro de uma CDS View, e por que não podemos usar sy-datum?  
R: A variável correta é $session.system\_date. Não podemos usar sy-datum porque CDS Views são objetos de dicionário de dados (DDL) que residem no banco de dados, enquanto sy-datum é uma variável de memória do tempo de execução ABAP. O banco de dados não tem acesso à memória do servidor de aplicação.  
Q2: Para que serve a função dats\_days\_between e qual o tipo de dado que ela retorna?  
R: Ela calcula a diferença exata em dias entre duas datas fornecidas. Retorna um valor inteiro (Integer). É a forma padrão e performática de calcular durações, idades ou atrasos diretamente no banco de dados.  
Q3: Se eu precisar transformar um código de status ('A', 'X') em uma cor para o Fiori (3, 1), qual comando devo usar e qual o benefício disso?  
R: Deve-se usar a expressão CASE. O benefício é que a lógica de apresentação ("Se A então Verde") fica centralizada na View. A interface de usuário (Fiori) apenas lê o valor numérico resultante e aplica a cor, sem precisar repetir a regra de negócio no JavaScript do frontend.  
Q4: O que acontece se eu tentar usar a função SUM( price ) em uma CDS View sem adicionar uma cláusula GROUP BY?  
R: Ocorrerá um erro de sintaxe. Funções de agregação condensam linhas. Se houver outros campos na seleção que não estão sendo agregados (como ID do Cliente), o banco de dados precisa saber como agrupar esses campos para calcular a soma correta para cada grupo.