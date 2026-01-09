# Jornada de Aprendizagem SAP ABAP Moderno

![Infográfico - A Evolução do ABAP](./Infografico_Geral_A_Evolucao_do_ABAP.png)

> **Este repositório é um guia completo para: desenvolvedores com conhecimento básico em linguagens como Javascript, Java, C#, Python, etc, profissionais que trabalham com SAP e desejam aprender ABAP ou desenvolvedores ABAP experientes que querem se atualizar com práticas modernas. Seja você uma dessas pessoas, você está no repositório certo. Este guia oferece um caminho estruturado e prático para facilitar a sua aprendizagem de maneira clara e objetiva, indo do nível básico ao avançado para que você aprofunde seus conhecimentos. Sendo a escolha perfeita para quem deseja adquirir habilidades fundamentais em programação ABAP, especialmente voltadas para ambientes modernos como **SAP BTP**, **SAP S/4HANA**, e **ABAP Cloud**.**


## 📚Conteúdo Programático

### 🔹[Módulo 1: Programação ABAP Básica](./modulo-01/README.md)
- Introdução ao ambiente de desenvolvimento ABAP Cloud
- Criação de pacotes, objetos e aplicações simples ("Hello World")
- Estrutura de software e logística
- Técnicas básicas de programação e uso de classes locais
- Leitura e manipulação de dados em banco de dados
- Uso de tabelas internas complexas
- Atualizações via Business Objects com EML
- Introdução ao modelo de programação RESTful (RAP)

### 🔹[Módulo 2: Modelagem de Dados com ABAP Dictionary e CDS](./modulo-02/README.md)
- Conceitos fundamentais do ABAP Dictionary
- Objetos principais de modelagem de dados
- Introdução ao ABAP Core Data Services (CDS)


### 🔹[Módulo 3: Introdução à Criação de um Aplicativo SAP Fiori Elements com Base em um Serviço OData V4 RAP](./modulo-03/README.md)
- Visão geral do SAP Fiori Elements para OData V4
- Introdução ao ABAP RESTful Application Programming Model (RAP)
- Configuração de anotações ABAP CDS no backend
- Exploração das ferramentas SAP Fiori Tools
- Compreensão dos floorplans do SAP Fiori Elements
- Configuração de relatórios de lista e páginas de objeto
- Configuração de tabelas
- Conceito de navegação em apps SAP Fiori Elements
- Configuração de páginas de objeto
- Modelo de programação flexível
- Implantação da aplicação SAP Fiori Elements

### 🔹[Módulo 4: Programação ABAP Intermediária](./modulo-04/README.md)
- Testes e análise de código com ABAP Test Cockpit (ATC)
- Criação de testes unitários e uso de ABAP Profiling
- Tipos de dados e conversões
- Manipulação de campos de caracteres
- Otimização de desempenho com Code Pushdown e tabelas internas
- Verificações de autorização
- Programação orientada a objetos eficaz
- Tratamento de exceções e documentação de código

### 🔹[Módulo 5: Praticando a extensibilidade do Clean Core para SAP S/4HANA Cloud](./modulo-05/README.md)
- Introdução ao SAP S/4HANA Cloud, Extensões e Clean Core
- Experiência do Usuário de Nível de Consumidor
- ABAP Cloud em Profundidade
- Modelo de Extensibilidade do SAP S/4HANA Cloud
- Considerações Especiais para SAP S/4HANA Cloud Private Edition e On-Premise

## Organização e Nomenclatura dos Arquivos

Para garantir a organização, a rastreabilidade e a sequência lógica de leitura dos arquivos do projeto, foi então definida a seguinte **Regra de Nomenclatura Hierárquica**: **`DD.SS_Slug_Descritivo`**

1. **DD (Diretório Principal)**: Dois dígitos representando a pasta raiz.
2. **SS (Subdiretório)**: Dois dígitos representando a subpasta.
3. **Slug Descritivo**: O nome original ou simplificado do tópico, separado por underlines para legibilidade.

## Como Contribuir

Contribuições são bem-vindas! Se você encontrar erros, tiver sugestões de melhoria ou quiser adicionar novo conteúdo, siga estes passos:

1.  **Fork** este repositório.
2.  Crie uma nova **branch** para sua feature (`git checkout -b feature/nova-feature`).
3.  Faça suas alterações e **commit** (`git commit -m 'feat: Adiciona nova feature'`).
4.  Envie para a sua branch (`git push origin feature/nova-feature`).
5.  Abra um **Pull Request**.

## Links Úteis
  
  ### Documentação Oficial
  - [ABAP - Keyword Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENABAP.html)
  - [ABAP RESTful Application Programming Model](https://help.sap.com/docs/abap-cloud/abap-rap/abap-restful-application-programming-model?locale=en-US&version=sap_btp)

  ### Aprenda Praticando - Tutoriais Oficiais da SAP
  - [Tutorial SAP - Crie um aplicativo de viagem com SAP Fiori Elements baseado no serviço OData V4 RAP](https://developers.sap.com/tutorials/abap-environment-extend-cds-view.html)
  - [Tutorial SAP - Estenda um Core Data Services Personalizado no Ambiente ABAP](https://developers.sap.com/tutorials/abap-environment-extend-cds-view.html)
  - [Tutorial SAP - Crie um aplicativo de Manutenção de Tabelas baseado em SAP Fiori](https://developers.sap.com/mission.abap-dev-factory-calendar.html)
  - [Tutorial SAP - [RAP100] - Construa um App SAP Fiori Usando o ABAP RESTful Application Programming Model](https://developers.sap.com/mission.abap-dev-factory-calendar.html)
  - [Tutorial SAP - Crie um aplicativo de Manutenção de Tabelas baseado em SAP Fiori](https://developers.sap.com/mission.abap-dev-factory-calendar.html)
  - [Tutorial SAP - Use o Mockserver e a Biblioteca de Teste OPA com SAP Fiori Elements para aplicações OData V4](https://developers.sap.com/group.fiori-elements-mockserver-opa.html)
  - [Tutorial SAP - Trabalhe com projetos de adaptação SAPUI5 para fazer alterações e estender o código-fonte de uma variante de aplicação SAP Fiori no SAP S/4HANA Cloud Public Edition](https://developers.sap.com/group.sapui5-adaptation-projects.html)

---

_Todo conteúdo presente nesse repositório foi gerado pelo Notebook LM com base na jornada de aprendizagem [**"Acquiring Core ABAP Skills"**](https://learning.sap.com/learning-journeys/acquire-core-abap-skills) da plataforma SAP Learning (plataforma de aprendizagem oficial da SAP)._