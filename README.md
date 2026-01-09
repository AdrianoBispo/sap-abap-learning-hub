# Guia de Estudo SAP ABAP Moderno

![Infográfico - A Evolução do ABAP](./Infografico_Geral_A_Evolucao_do_ABAP.png)

> **Este repositório é um guia completo para: desenvolvedores com conhecimento básico em linguagens como Javascript, Java, C#, Python, etc, profissionais que trabalham com SAP e desejam aprender ABAP ou desenvolvedores ABAP experientes que querem se atualizar com práticas modernas. Seja você uma dessas pessoas, você está no repositório certo. Este guia oferece um caminho estruturado e prático para facilitar a sua aprendizagem de maneira clara e objetiva, indo do nível básico ao avançado para que você aprofunde seus conhecimentos. Sendo a escolha perfeita para quem deseja adquirir habilidades fundamentais em programação ABAP, especialmente voltadas para ambientes modernos como **SAP BTP**, **SAP S/4HANA**, e **ABAP Cloud**.**

## Estrutura do Repositório

O conteúdo está organizado em módulos progressivos, do básico ao avançado:

| Módulo | Descrição Detalhada |
| :--- | :--- |
| **[Módulo 1: Iniciando a Jornada no Desenvolvimento ABAP Moderno](./modulo-01/README.md)** | _Neste módulo você verá uma introdução abrangente à linguagem de programação ABAP Moderna, voltada para iniciantes. Aqui você aprenderá a: criar pacotes, objetos e aplicações simples. Também irá aprender sobre: estrutura de software e logística; técnicas básicas de programação e uso de classes locais; leitura e manipulação de dados em banco de dados; uso de tabelas internas complexas; atualizações via Business Objects com EML. E no fim, será introduzido ao conceito **ABAP RESTful Application Programming Model (RAP)**._ |
| **[Módulo 2: Modelagem de Dados com ABAP Dictionary e CDS](./modulo-02/README.md)** | _Neste módulo você irá aprender sobre: os conceitos fundamentais do **ABAP Dictionary** e a como definir tabelas de banco de dados utilizando o mesmo; criação e gerenciamento de **views** utilizando **ABAP Core Data Services (CDS)**; modelagem de relacionamentos e associações entre objetos; como aplicar lógica SQL e enriquecer views com **Metadata Extensions**; proteger dados contra acessos não autorizados; os objetos principais de modelagem de dados e o ciclo de vida dos serviços; como estender modelos de dados externos._ |
| **[Módulo 3: Introdução à Criação de um Aplicativo SAP Fiori Elements com Base em um Serviço OData V4 RAP](./modulo-03/README.md)** | _Neste mṕodulo você aprenderá a: desenvolver aplicações **SAP Fiori Elements** com base em serviços **OData V4** criados via **RAP**, utilizando ferramentas modernas como o **SAP Business Application Studio** e **SAP Fiori Tools**._ |
| **[Módulo 4: Programação ABAP Intermediária](./modulo-04/README.md)** | _Neste módulo você irá aprender sobre: testes e análise de código com **ABAP Test Cockpit (ATC)**; criação de testes unitários e uso de **ABAP Profiling**; tipos de dados e conversões; manipulação de campos de caracteres; otimização de desempenho com **Code Pushdown** e **Tabelas Internas**; verificações de autorização; programação orientada a objetos eficaz; tratamento de exceções e documentação de código._ |
| **[Módulo 5: Praticando a extensibilidade do Clean Core para SAP S/4HANA Cloud](./modulo-05/README.md)** | _E, por fim, neste módulo você irá aprender como desenvolver extensões que: preservem a integridade do núcleo do sistema (clean core); facilitem atualizações futuras sem comprometer funcionalidades personalizadas; sejam compatíveis com os modelos de desenvolvimento modernos da SAP, como o **ABAP Cloud Development Model**._ |

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

## Licença

Este projeto é distribuído sob a licença MIT. Veja o arquivo `LICENSE` para mais detalhes.

---

_Todo conteúdo presente nesse repositório foi gerado pelo Notebook LM com base nos cursos da jornada de aprendizagem [**"Acquiring Core ABAP Skills"**](https://learning.sap.com/learning-journeys/acquire-core-abap-skills) da plataforma SAP Learning (plataforma de aprendizagem oficial da SAP)._