# **Módulo 01: Programação ABAP Básica**

## **Aula 07: Implementando Atualizações de Banco de Dados usando Business Objects**

### **🎯 Objetivos de Aprendizagem**

Ao final desta aula, o estudante deverá ser capaz de:

1. Compreender profundamente a arquitetura transacional do RAP e por que comandos de atualização direta SQL (INSERT, UPDATE, DELETE) são estritamente proibidos em cenários de negócio modernos.  
2. Dominar a sintaxe e a semântica da **EML (Entity Manipulation Language)** para interagir programaticamente com Business Objects, tanto para leitura quanto para modificação.  
3. Aplicar corretamente os comandos **MODIFY ENTITIES** para operações de criação, atualização, exclusão e execução de ações customizadas.  
4. Gerenciar a transacionalidade utilizando **COMMIT ENTITIES**, compreendendo a distinção entre o *Transactional Buffer* e a persistência física no banco de dados.  
5. Interpretar e manipular as estruturas de retorno padrão **FAILED**, **MAPPED** e **REPORTED** para tratamento robusto de erros.

### **1\. O Fim do SQL Direto (INSERT/UPDATE)**

No paradigma antigo do ABAP Clássico, a persistência de dados era direta e muitas vezes bruta. Se um desenvolvedor precisasse criar um Pedido de Venda, ele frequentemente escrevia um comando INSERT INTO vbap VALUES .... Embora eficiente do ponto de vista puramente técnico (banco de dados), essa abordagem é catastrófica para a integridade do negócio.

O Problema do "Bypass":  
Ao inserir diretamente na tabela, você está ignorando ("bypassing") toda a inteligência da aplicação.

* **Validações:** O código SQL não sabe que "Data de Entrega não pode ser menor que Data de Criação".  
* **Cálculos Automáticos:** O SQL não recalcula o valor total do pedido quando você insere um item.  
* **Autorizações:** O SQL não verifica se o usuário tem permissão para criar pedidos naquela Organização de Vendas.  
* **Logs e Auditoria:** O SQL não gera documentos de modificação (Change Documents) automaticamente.

A Solução (RAP):  
No modelo RAP, aplicamos o princípio de Encapsulamento. A tabela do banco de dados é privada do Business Object. Ninguém de fora pode tocá-la. Para modificar dados, você deve "pedir" ao Business Object. O BO atua como um guardião, garantindo que todas as regras sejam cumpridas antes que qualquer dado seja gravado.

### **2\. O que é EML (Entity Manipulation Language)?**

O EML não é uma nova linguagem separada, mas uma extensão poderosa da sintaxe ABAP nativa, desenhada especificamente para o modelo RAP. Enquanto o SQL foca em *tabelas*, o EML foca em *entidades* e *comportamentos*.

* **ABAP SQL (SELECT):** Utilizado para **Leitura** de dados em massa. O RAP permite (e encoraja) o uso de SQL para leitura direta de CDS Views para relatórios, pois leitura não afeta a integridade.  
* **EML (MODIFY, READ):** Utilizado para **Transações**. É a única porta de entrada para alterar dados ou executar lógicas de negócio (Ações) em um BO.

**Vantagens do EML:**

1. **Tipagem Forte:** O compilador verifica se os campos existem na entidade CDS.  
2. **Abstração:** Você não precisa saber em quais tabelas físicas o BO grava (pode ser uma, podem ser dez). O EML abstrai essa complexidade.  
3. **Buffer Transacional:** O EML trabalha em memória. As alterações não vão para o banco imediatamente; elas ficam num buffer gerenciado pelo framework até o commit.

### **3\. A Sintaxe do MODIFY ENTITIES**

O comando MODIFY ENTITIES é o canivete suíço do RAP. Ele permite Criar (CREATE), Alterar (UPDATE), Deletar (DELETE) e Executar Ações (EXECUTE).

#### **Estrutura Geral e Parâmetros de Retorno**

MODIFY ENTITIES OF Nome\_Da\_Definicao\_De\_Comportamento  
  ENTITY Nome\_Da\_Entidade  
    
  " Operação 1: Criar novas instâncias  
  CREATE FROM lt\_novos\_dados   
    
  " Operação 2: Atualizar instâncias existentes  
  UPDATE FROM lt\_dados\_alterados  
    
  " Operação 3: Executar uma Ação (Ex: Aprovar)  
  EXECUTE aceitar\_viagem FROM lt\_chaves\_acao

  " Retornos Padronizados (Sempre use\!)  
  FAILED   DATA(ls\_failed)  
  REPORTED DATA(ls\_reported)  
  MAPPED   DATA(ls\_mapped).

* **FAILED:** Contém as chaves das linhas que falharam. O framework preenche isso automaticamente se uma validação falhar.  
* **REPORTED:** Contém as mensagens de erro (T100) associadas às falhas. É aqui que você descobre *por que* falhou (ex: "Cliente inválido").  
* **MAPPED:** (Usado no CREATE) Contém o mapeamento entre o ID temporário (%cid) e o ID final gerado pelo sistema, vital para numeração tardia (Late Numbering).

### **4\. Transacionalidade: COMMIT ENTITIES**

Uma diferença crítica entre o ABAP Clássico e o RAP é o gerenciamento da transação.

No clássico, COMMIT WORK disparava a gravação física imediata. No RAP, o processo é dividido em duas fases distintas:

1. **Interaction Phase (Fase de Interação):** Onde ocorrem os MODIFY ENTITIES. Os dados são validados e colocados no **Transactional Buffer** (memória do servidor de aplicação). Nada foi gravado no banco ainda.  
2. **Save Phase (Fase de Salvamento):** Disparada pelo comando COMMIT ENTITIES. O framework pega os dados do buffer, executa validações finais ("Save Validations") e, se tudo estiver ok, persiste no banco de dados.

**Sintaxe de Commit:**

" Commit simples (Não recomendado se você precisa tratar erros de salvamento)  
COMMIT ENTITIES.

" Commit com Resposta (Recomendado)  
" Permite saber se houve erro durante o processo de gravação final  
COMMIT ENTITIES   
  RESPONSE OF Nome\_Do\_BDEF   
  FAILED DATA(ls\_commit\_failed)   
  REPORTED DATA(ls\_commit\_reported).

### **5\. Exemplo Prático: Criando e Atualizando Viagens via EML**

Neste exemplo expandido, vamos simular um programa que cria uma viagem e, logo em seguida, tenta atualizar um campo dela, demonstrando a interação completa com o buffer.

CLASS zcl\_eml\_demo DEFINITION  
  PUBLIC  
  FINAL  
  CREATE PUBLIC .

  PUBLIC SECTION.  
    INTERFACES if\_oo\_adt\_classrun .  
  PROTECTED SECTION.  
  PRIVATE SECTION.  
ENDCLASS.

CLASS zcl\_eml\_demo IMPLEMENTATION.

  METHOD if\_oo\_adt\_classrun\~main.

    " \---------------------------------------------------------------------  
    " PASSO 1: Preparação dos Dados para CRIAÇÃO  
    " \---------------------------------------------------------------------  
    " O %cid (Content ID) é um identificador temporário obrigatório (string)  
    " que permite referenciar esta linha antes dela ter um número final.  
    " O %control indica quais campos estamos fornecendo.  
      
    DATA(lt\_travel\_create) \= VALUE /dmo/t\_travel\_create\_in(  
      ( %cid \= 'TEMP\_ID\_001'  
        agency\_id     \= '070001'  
        customer\_id   \= '000001'  
        begin\_date    \= cl\_abap\_context\_info=\>get\_system\_date( )  
        end\_date      \= cl\_abap\_context\_info=\>get\_system\_date( ) \+ 10  
        booking\_fee   \= '20.00'  
        currency\_code \= 'EUR'  
        description   \= 'Viagem criada via EML'  
        status        \= 'O' " Open  
        %control-agency\_id     \= if\_abap\_behv=\>mk-on  
        %control-customer\_id   \= if\_abap\_behv=\>mk-on  
        %control-begin\_date    \= if\_abap\_behv=\>mk-on  
        %control-end\_date      \= if\_abap\_behv=\>mk-on  
        %control-booking\_fee   \= if\_abap\_behv=\>mk-on  
        %control-currency\_code \= if\_abap\_behv=\>mk-on  
        %control-description   \= if\_abap\_behv=\>mk-on  
        %control-status        \= if\_abap\_behv=\>mk-on  
       )  
    ).

    " \---------------------------------------------------------------------  
    " PASSO 2: Executar o MODIFY (Interaction Phase)  
    " \---------------------------------------------------------------------  
    " Aqui, o framework executa validações. Se a agência não existir,  
    " ls\_failed será preenchido.  
      
    MODIFY ENTITIES OF /DMO/I\_Travel\_M  
      ENTITY Travel  
      CREATE FROM lt\_travel\_create  
        
      " Captura de retornos  
      FAILED DATA(ls\_failed)  
      REPORTED DATA(ls\_reported)  
      MAPPED DATA(ls\_mapped).

    IF ls\_failed IS NOT INITIAL.  
      " Tratamento de Erro na Criação  
      " Leríamos ls\_reported para mostrar a mensagem ao usuário.  
      out-\>write( 'Erro: Falha na criação da viagem (Validação).' ).  
        
      " Exemplo de como ler mensagens de erro do REPORTED  
      LOOP AT ls\_reported-travel INTO DATA(ls\_msg).  
         DATA(lv\_text) \= ls\_msg-%msg-\>get\_text( ).  
         out-\>write( |Detalhe: { lv\_text }| ).  
      ENDLOOP.  
      RETURN.   
    ENDIF.

    " Se chegou aqui, a criação foi bem sucedida NO BUFFER.  
    " Podemos acessar a chave gerada através da estrutura MAPPED.  
    DATA(lv\_new\_travel\_id) \= ls\_mapped-travel\[ 1 \]-travel\_id.  
    out-\>write( |Viagem criada no buffer com ID: { lv\_new\_travel\_id }| ).

    " \---------------------------------------------------------------------  
    " PASSO 3: Atualização na mesma transação (Chaining)  
    " \---------------------------------------------------------------------  
    " Vamos aumentar a taxa de reserva para 50.00.  
      
    MODIFY ENTITIES OF /DMO/I\_Travel\_M  
      ENTITY Travel  
      UPDATE FIELDS ( booking\_fee ) " Só atualiza este campo  
      WITH VALUE \#( ( travel\_id   \= lv\_new\_travel\_id  
                      booking\_fee \= '50.00' ) )  
      FAILED DATA(ls\_update\_failed)  
      REPORTED DATA(ls\_update\_reported).

    IF ls\_update\_failed IS NOT INITIAL.  
       out-\>write( 'Erro ao atualizar a taxa.' ).  
       RETURN.  
    ENDIF.

    " \---------------------------------------------------------------------  
    " PASSO 4: Persistir (Save Phase)  
    " \---------------------------------------------------------------------  
    " Até agora, nada está no banco. O COMMIT efetiva a transação.  
      
    COMMIT ENTITIES  
      RESPONSE OF /DMO/I\_Travel\_M  
      FAILED DATA(ls\_commit\_failed)  
      REPORTED DATA(ls\_commit\_reported).

    IF ls\_commit\_failed IS INITIAL.  
      out-\>write( 'Sucesso Total\! Dados gravados no banco de dados.' ).  
    ELSE.  
      out-\>write( 'Erro durante a fase de salvamento (Save Sequence).' ).  
    ENDIF.

  ENDMETHOD.

ENDCLASS.

### **🧠 Material para Estudo (Flashcards & Resumo)**

#### **Glossário Técnico Expandido**

* **EML (Entity Manipulation Language):** Extensão da linguagem ABAP utilizada para interagir com Business Objects no modelo RAP. Permite operações transacionais (MODIFY) e leitura com privilégios (READ).  
* **Business Object (BO):** Representação virtual de uma entidade de negócio (ex: Viagem) que encapsula dados (CDS) e comportamento (Behavior Pool).  
* **%cid (Content ID):** Identificador temporário alfanumérico atribuído pelo consumidor (você) durante uma operação de CREATE. Ele serve para identificar a linha no buffer antes que o banco de dados gere a chave primária definitiva.  
* **Transactional Buffer:** Área de memória no servidor de aplicação onde o RAP armazena o estado das entidades modificadas durante a *Interaction Phase*. O SQL INSERT/UPDATE só ocorre quando este buffer é processado na *Save Phase*.  
* **FAILED Structure:** Tabela de retorno do EML que contém as chaves das instâncias que falharam na operação. Se uma linha está em FAILED, ela não será salva.  
* **REPORTED Structure:** Tabela de retorno do EML que contém objetos de mensagem (referências a T100) explicando a causa do erro. Geralmente ligada à chave da instância falha.  
* **MAPPED Structure:** Tabela de retorno vital para operações de criação (CREATE). Ela mapeia o %cid (ID temporário) para a chave real gerada pelo BO (ex: número do pedido).

#### **Pontos de Atenção (Clássico vs. RAP)**

| Operação | ABAP Clássico | ABAP Moderno (RAP) |
| :---- | :---- | :---- |
| **Criar Dados** | INSERT ztable FROM ls\_data. (Direto, sem validação BO) | MODIFY ENTITIES ... CREATE. (Passa pelo BO) |
| **Alterar Dados** | UPDATE ztable SET ... | MODIFY ENTITIES ... UPDATE. |
| **Persistir** | COMMIT WORK. (Síncrono/Assíncrono direto) | COMMIT ENTITIES. (Dispara Save Sequence) |
| **Tratamento de Erro** | sy-subrc \<\> 0\. | Parâmetros FAILED e REPORTED. |
| **Identificação** | Chave da tabela (obrigatória) | %cid (criação) ou Chave (atualização) |

### **📝 Quiz de Fixação**

Q1: Qual é a principal vantagem de usar MODIFY ENTITIES em vez de um INSERT direto na tabela do banco de dados?  
R: O MODIFY ENTITIES aciona toda a lógica do Business Object, incluindo validações, determinações (cálculos automáticos) e verificações de autorização. O INSERT direto ignora ("bypassa") essas regras, podendo gerar inconsistência nos dados (dados sujos).  
Q2: Para que serve o parâmetro %cid em uma operação de criação EML?  
R: O %cid (Content ID) serve para identificar unicamente uma linha sendo criada no buffer transacional antes que o banco de dados gere a chave primária definitiva. Ele é essencial para criar hierarquias (ex: criar Cabeçalho e Item juntos) onde o Item precisa referenciar o Cabeçalho que ainda não tem ID final.  
Q3: O comando MODIFY ENTITIES grava os dados imediatamente no banco de dados?  
R: Não. Ele apenas atualiza o Transactional Buffer na memória da sessão ABAP. A gravação física no banco de dados só ocorre quando o comando COMMIT ENTITIES é executado com sucesso e todas as validações de salvamento são aprovadas.  
Q4: O que devo verificar para saber se uma operação EML (como UPDATE) foi bem-sucedida?  
R: Você deve verificar se a estrutura de retorno FAILED está inicial (vazia). Se FAILED contiver registros, a operação falhou para aquelas instâncias específicas. Não confie em sy-subrc para comandos EML.