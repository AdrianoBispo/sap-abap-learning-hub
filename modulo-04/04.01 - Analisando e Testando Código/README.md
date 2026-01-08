# **Módulo 04: Aprofundando o Conhecimento em Programação ABAP**

## **Aula 01: Analisando e Testando Código**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Diferenciar tecnicamente e conceitualmente **Análise Estática** (ATC/Code Inspector) de **Análise Dinâmica** (Unit Tests, Profiling), compreendendo o momento certo de aplicar cada uma no ciclo de vida do desenvolvimento (ALM).  
2. Utilizar o **ABAP Test Cockpit (ATC)** para identificar e corrigir proativamente problemas de sintaxe moderna, gargalos de performance, falhas de segurança e conformidade com o *Clean Core*.  
3. Escrever classes de **Teste Unitário (ABAP Unit)** robustas, aplicando o padrão **AAA (Arrange, Act, Assert)** e compreendendo o ciclo de vida dos métodos de configuração (setup, teardown).  
4. Executar e interpretar rastreamentos de **ABAP Profiling** no Eclipse para identificar o consumo de recursos (CPU vs. Banco de Dados) e otimizar rotinas críticas.

### **1\. A Tríade da Qualidade de Código: Do "Funciona" para o "É Robusto"**

No desenvolvimento de software moderno, especialmente em ambientes ERP críticos como o S/4HANA, "funcionar na minha máquina" não é suficiente. Um código pode produzir o resultado correto hoje, mas ser impossível de manter amanhã ou derrubar o servidor quando o volume de dados aumentar.

A qualidade de código no ABAP apoia-se em três pilares fundamentais, frequentemente automatizados em esteiras de CI/CD (Integração Contínua):

1. **Conformidade (Compliance) \-\> ATC:** O código segue as regras da empresa e da SAP? Ele é seguro? Ele está pronto para a nuvem?  
2. **Correção (Correctness) \-\> ABAP Unit:** O código faz exatamente o que a regra de negócio pede? Se eu mudar uma linha hoje, eu quebro uma regra antiga (Regressão)?  
3. **Performance \-\> ABAP Profiling:** O código é eficiente? Ele consome memória excessiva ou faz chamadas desnecessárias ao banco?

### **2\. Análise Estática: ABAP Test Cockpit (ATC)**

O ATC é o "inspetor de qualidade" automatizado. Ele utiliza a engine do *Code Inspector (SCI)* para ler seu código fonte sem executá-lo, procurando por padrões conhecidos de má programação, vulnerabilidades de segurança ou sintaxe obsoleta.

#### **O que o ATC verifica (Categorias Principais):**

* **Sintaxe e Modernização:** Uso de comandos obsoletos (ex: MOVE, COMPUTE, SELECT \*) em vez das novas sintaxes (Inline, Expressions).  
* **Performance (HANA):** Detecta *SELECTs dentro de LOOPs* (o pecado capital da performance), leitura de tabelas inteiras sem filtro e índices ausentes.  
* **Segurança:** Falta de verificação de autorização (AUTHORITY-CHECK) antes de ações críticas ou vulnerabilidades de injeção de SQL (SQL Injection) em SQL dinâmico.  
* **Robustez:** Variáveis declaradas mas não usadas (Dead Code), ou tipos incompatíveis que podem causar *Dumps* em tempo de execução.  
* **Cloud Readiness:** Verifica se o código utiliza objetos não liberados ou acessa diretamente o sistema operacional, o que é proibido no ABAP Cloud.

#### **Gerenciando Descobertas e Isenções (Pragmas e Pseudo-Comentários)**

Às vezes, o ATC aponta um erro que, naquele contexto específico, é intencional ou inevitável.

* **Quick Fixes (Ctrl+1):** No ADT, muitos erros (como falta de ordenação no SELECT) podem ser corrigidos automaticamente pela ferramenta.  
* **Pragmas:** Para silenciar um aviso falso-positivo, usamos pragmas (comandos \#\#...). Exemplo: Se você declarar uma variável que propositalmente não será usada agora:  
  DATA(lv\_dummy) \= 'Teste' \#\#NO\_TEXT. " Ignora aviso de hardcoded text

### **3\. Análise Dinâmica: ABAP Unit Tests**

Enquanto o ATC olha a *forma*, o ABAP Unit olha a *função*. O Teste Unitário é uma rede de segurança que permite refatorar e evoluir o sistema sem medo de quebrar funcionalidades existentes.

#### **Anatomia de uma Classe de Teste**

As classes de teste são **Classes Locais** especiais definidas dentro da classe global (na aba "Local Test Classes" do ADT), marcadas com a cláusula FOR TESTING. Elas não são transportadas para produção como código executável, mas garantem a qualidade antes do transporte.

**Configurações da Classe (RISK LEVEL e DURATION):**

* **RISK LEVEL HARMLESS:** O teste é seguro. Não faz COMMIT no banco, não altera configurações do sistema. (Obrigatório para testes automáticos).  
* **RISK LEVEL DANGEROUS/CRITICAL:** O teste pode alterar dados reais. Geralmente evitado ou usado apenas em ambientes de sandbox controlados.  
* **DURATION SHORT:** O teste deve rodar em frações de segundo. Se demorar, o framework aborta.

#### **O Ciclo de Vida do Teste (Fixtures)**

Para garantir que cada teste seja independente (isolado), o framework oferece métodos especiais:

1. **class\_setup:** Roda uma vez antes de todos os testes da classe (ex: carregar dados estáticos pesados).  
2. **setup:** Roda **antes de cada método** de teste. Usado para criar uma nova instância da classe a ser testada (limpar a memória).  
3. **teardown:** Roda **após cada método** de teste. Usado para limpar recursos ou desfazer alterações.  
4. **class\_teardown:** Roda uma vez ao final de tudo.

### **4\. Análise de Performance: ABAP Profiling**

Às vezes, o código está correto e passa no ATC, mas o usuário reclama de lentidão. O "olhômetro" é péssimo para julgar performance. O **ABAP Profiler** (integrado ao ADT) é a ferramenta científica para isso.

**Como funciona:**

1. Você inicia o Profiling no ADT para um usuário ou processo.  
2. Executa a ação no sistema (Fiori, Console, Transação).  
3. Para o Profiling e analisa o arquivo de rastreamento (Trace File).

**O que analisar no Trace:**

* **Hit List:** Quais métodos foram chamados mais vezes? (Ex: um método pequeno chamado 1 milhão de vezes pode ser o gargalo).  
* **Gross Time vs. Net Time:** Tempo total gasto no método (incluindo chamadas filhas) vs. tempo gasto processando a lógica do próprio método.  
* **Database Accesses:** Mostra exatamente quais SELECTs foram feitos, quantas linhas retornaram e quanto tempo o banco demorou. É aqui que descobrimos o problema "N+1" (muitos selects pequenos).

### **5\. Exemplo Prático: Teste Unitário com Cenário Negativo**

Vamos expandir o exemplo da calculadora para incluir um teste de sucesso e um teste de falha (cenário negativo), demonstrando o uso de asserções.

**Classe de Produção (zcl\_calc):**

CLASS zcl\_calc DEFINITION PUBLIC FINAL CREATE PUBLIC.  
  PUBLIC SECTION.  
    "\! Método para dividir dois números  
    "\! @parameter iv\_a | Dividendo  
    "\! @parameter iv\_b | Divisor  
    "\! @returning rv\_result | Resultado da divisão  
    "\! @raising cx\_sy\_zerodivide | Se o divisor for zero  
    METHODS dividir  
      IMPORTING iv\_a TYPE i iv\_b TYPE i  
      RETURNING VALUE(rv\_result) TYPE p LENGTH 10 DECIMALS 2  
      RAISING   cx\_sy\_zerodivide.  
ENDCLASS.

CLASS zcl\_calc IMPLEMENTATION.  
  METHOD dividir.  
    " Se iv\_b for 0, o kernel ABAP dispara cx\_sy\_zerodivide automaticamente  
    rv\_result \= iv\_a / iv\_b.  
  ENDMETHOD.  
ENDCLASS.

**Classe de Teste (Aba "Local Test Classes" no ADT):**

\*"\* use this source file for your ABAP unit test classes

CLASS ltcl\_test\_calc DEFINITION FINAL FOR TESTING  
  DURATION SHORT  
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.  
    " CUT \= Class Under Test (O objeto que vamos testar)  
    DATA: mo\_cut TYPE REF TO zcl\_calc.

    " Fixture: Prepara o ambiente antes de CADA teste  
    METHODS setup.

    " Casos de Teste  
    METHODS test\_divisao\_sucesso FOR TESTING.  
    METHODS test\_divisao\_por\_zero FOR TESTING.  
ENDCLASS.

CLASS ltcl\_test\_calc IMPLEMENTATION.

  METHOD setup.  
    " Garante uma instância nova e limpa para cada teste  
    mo\_cut \= NEW \#( ).  
  ENDMETHOD.

  METHOD test\_divisao\_sucesso.  
    " Padrão AAA: Arrange (Preparar), Act (Agir), Assert (Verificar)  
      
    " Act  
    TRY.  
        DATA(lv\_result) \= mo\_cut-\>dividir( iv\_a \= 10 iv\_b \= 2 ).  
    CATCH cx\_sy\_zerodivide.  
        cl\_abap\_unit\_assert=\>fail( msg \= 'Não deveria lançar exceção aqui' ).  
    ENDTRY.

    " Assert  
    cl\_abap\_unit\_assert=\>assert\_equals(  
      act \= lv\_result  " Actual (O que o código devolveu)  
      exp \= 5          " Expected (O que a matemática diz)  
      msg \= 'Divisão de 10 / 2 deve ser 5'  
      tol \= '0.01'     " Tolerância para números decimais  
    ).  
  ENDMETHOD.

  METHOD test\_divisao\_por\_zero.  
    " Testando se o código FALHA corretamente quando deve falhar  
      
    TRY.  
        mo\_cut-\>dividir( iv\_a \= 10 iv\_b \= 0 ).  
          
        " Se chegou aqui, o código NÃO lançou erro, o que está ERRADO para este teste  
        cl\_abap\_unit\_assert=\>fail( msg \= 'Deveria ter lançado exceção de divisão por zero' ).  
          
    CATCH cx\_sy\_zerodivide.  
        " Se caiu aqui, o teste passou\! O erro era esperado.  
    ENDTRY.  
  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **Static Analysis (Análise Estática):** Verificação do código fonte em repouso (sem execução). Foca em sintaxe, convenções, segurança e conformidade (Clean Core). Ferramenta: ATC.  
* **Dynamic Analysis (Análise Dinâmica):** Verificação do comportamento do código em execução. Foca em lógica de negócio, fluxo de dados e consumo de recursos. Ferramentas: ABAP Unit, Profiler.  
* **ATC (ABAP Test Cockpit):** Cockpit central de qualidade da SAP. Permite configurar variantes de verificação (Check Variants) e pode ser integrado ao sistema de transportes para bloquear código ruim.  
* **ABAP Unit:** Framework nativo de testes unitários (família xUnit). Permite a criação de testes de regressão automatizados que rodam em milissegundos.  
* **CUT (Code Under Test):** O objeto específico (classe/método) que está sendo testado.  
* **Test Fixture (Setup/Teardown):** Métodos especiais que configuram o estado inicial antes de um teste e limpam o estado após o teste, garantindo isolamento.  
* **Code Coverage (Cobertura de Código):** Métrica que indica qual porcentagem do seu código produtivo foi efetivamente executada pelos seus testes unitários. Uma cobertura alta reduz o risco de bugs não detectados.  
* **Mocking (Test Double):** Técnica de substituir dependências complexas (como acesso a banco de dados ou chamadas de API externa) por objetos simulados (Mocks) dentro do teste unitário, para testar a lógica isoladamente.

#### **Comparativo Detalhado: ATC vs ABAP Unit**

| Característica | ATC (Test Cockpit) | ABAP Unit |
| :---- | :---- | :---- |
| **Tipo de Análise** | Estática (Lê o código). | Dinâmica (Roda o código). |
| **O que detecta?** | Erros de sintaxe, performance estrutural, segurança, convenções de nome. | Lógica de negócio incorreta, bugs de cálculo, falhas de fluxo. |
| **Responsabilidade** | O sistema verifica automaticamente (Qualidade passiva). | O Desenvolvedor escreve os cenários (Qualidade ativa). |
| **Execução** | Na liberação da tarefa ou manual. | Manual (Ctrl+Shift+F10) ou agendada (Nightly). |
| **Bloqueio** | Pode impedir a liberação da Request (Transporte). | Erros de teste não bloqueiam transporte por padrão (mas deveriam). |

### **📝 Quiz de Fixação**

Q1: Qual é a função primária da classe cl\_abap\_unit\_assert e cite dois de seus métodos principais?  
R: Ela fornece os métodos estáticos para validar os resultados dos testes. Se a condição falhar, ela interrompe o teste e reporta o erro. Métodos principais: assert\_equals (verifica se dois valores são iguais) e assert\_bound (verifica se uma referência de objeto é válida/instanciada).  
Q2: O que significa definir uma classe de teste com RISK LEVEL HARMLESS e por que isso é importante?  
R: Significa que o teste é inofensivo e seguro para rodar em qualquer ambiente (inclusive produtivo, embora não recomendado). Ele garante que o teste não fará alterações persistentes no banco de dados (COMMIT) nem afetará configurações do sistema. Se o teste tentar fazer isso, o framework o abortará imediatamente.  
Q3: Durante uma análise de Profiling, você percebe que o tempo de banco de dados (Database Time) é muito maior que o tempo de ABAP (CPU Time). O que isso geralmente indica e onde você deve procurar o problema?  
R: Isso indica que a aplicação está gastando a maior parte do tempo esperando o banco de dados responder. O problema geralmente está em SELECTs ineficientes (falta de índice, leitura de colunas desnecessárias) ou, muito frequentemente, no problema "N+1" (fazer um SELECT dentro de um LOOP). O desenvolvedor deve olhar a aba "Database Accesses" do Profiler.  
Q4: Para silenciar um erro do ATC que você sabe ser um falso-positivo (ou uma exceção permitida), o que você deve usar no código?  
R: Você deve usar um Pragma (código iniciado por \#\#, ex: \#\#NO\_TEXT) ou um Pseudo-Comentário (comentário especial \#EC...), dependendo do tipo de mensagem de erro, para instruir o ATC a ignorar aquela linha específica.