# Apêndices da Dissertação de Mestrado

**Impacto dos Sistemas de Certificação de Qualidade nos Pequenos Produtores Agrícolas na Agricultura Sustentável em Portugal**

Ervaldo Jorge dos Santos (N.º 74273)
Mestrado em Gestão Sustentável de Espaços Rurais
Faculdade de Ciências e Tecnologia, Universidade do Algarve, 2025/2026
Orientação: Prof. Dr. Luís Miguel de Amorim Ferreira Fernandes Nunes

---

Este repositório reúne as versões integrais dos apêndices da dissertação, a base de dados do inquérito, o script R e o guia que explica cada passo da análise. No texto da dissertação, cada apêndice é referido pela sua letra (por exemplo, «ver Apêndice E»); essa letra corresponde ao ficheiro indicado abaixo.

## Apêndices

| Apêndice | Conteúdo | Ficheiro |
|---|---|---|
| A | Análise qualitativa da Secção 13 (pergunta aberta): transcrição, codificação, análise léxica e diagramas de rede | [Apendice_A_Analise_Qualitativa_Seccao13.pdf](Apendice_A_Analise_Qualitativa_Seccao13.pdf) |
| B | Recolha, validação e tratamento dos dados, incluindo o exemplar do formulário | [Apendice_B_Recolha_e_Tratamento_de_Dados.pdf](Apendice_B_Recolha_e_Tratamento_de_Dados.pdf) |
| C | Tabela de correspondência e matriz de cruzamento | [Apendice_C_Tabela_de_Correspondencia_e_Matriz_de_Cruzamento.pdf](Apendice_C_Tabela_de_Correspondencia_e_Matriz_de_Cruzamento.pdf) |
| D | Análise comparativa estrutural: gráficos aluviais e boxplots | [Apendice_D_Analise_Estrutural_Aluviais_e_Boxplots.pdf](Apendice_D_Analise_Estrutural_Aluviais_e_Boxplots.pdf) |
| E | Análise estatística inferencial: as 41 hipóteses nulas (H0.1–H0.41) e os resultados dos testes | [Apendice_E_Analise_Estatistica_Inferencial_H0.1_H0.41.pdf](Apendice_E_Analise_Estatistica_Inferencial_H0.1_H0.41.pdf) |
| F | Base de dados do inquérito (Excel): respostas, dados brutos, tabelas de cobertura e de correspondência, matriz de cruzamento e matriz de Spearman | [Apendice_F_Estudo_Matriz_Corrigida.xlsx](Apendice_F_Estudo_Matriz_Corrigida.xlsx) |
| G | Composição das medidas da matriz de cruzamento, por inquirido | [Apendice_G_Matriz_de_Cruzamento_por_Inquirido.pdf](Apendice_G_Matriz_de_Cruzamento_por_Inquirido.pdf) |

## Reprodução da análise

| Ficheiro | Conteúdo |
|---|---|
| [Script_R_Estudo_Matriz_Corrigida.R](Script_R_Estudo_Matriz_Corrigida.R) | Script R que reproduz, passo a passo, a matriz de cruzamento, os dados dos gráficos aluviais, os boxplots e todos os testes |
| [dados_brutos.csv](dados_brutos.csv) | Dados de entrada do script (67 inquiridos × 36 colunas); idêntico à folha «Dados brutos (R)» do Apêndice F |
| [Guia_Detalhado_Script_R_Passo_a_Passo.pdf](Guia_Detalhado_Script_R_Passo_a_Passo.pdf) | Guia que explica cada comando do script, os resultados obtidos e a sua leitura (Passos 0 a 13) |

Para reproduzir os resultados: colocar `Script_R_Estudo_Matriz_Corrigida.R` e `dados_brutos.csv` na mesma pasta, definir essa pasta como diretório de trabalho no R e executar o script. O guia indica, para cada passo, os valores que devem ser obtidos.

## Notas

- Cada apêndice em PDF é um documento autónomo, com paginação própria e com as suas próprias referências bibliográficas no final.
- Na base de dados (Apêndice F) e no ficheiro `dados_brutos.csv`, os inquiridos estão identificados apenas por número (1 a 67). A participação foi anónima e os dados são tratados de forma agregada, em conformidade com o RGPD.
- A análise estatística foi feita no R 4.6.1 (R Core Team, 2026), com nível de significância α = 0,05 e correção de Benjamini-Hochberg dentro de cada bloco de testes.
- A versão atual (v2.0) corresponde à dissertação com a **matriz de cruzamento corrigida**: cada pergunta entra numa só medida, as perguntas de caracterização (1.1–1.4) servem apenas como variáveis de grupo e as respostas são reescaladas para 1–5. As versões anteriores (v1.0 e v1.1) continuam disponíveis em *Releases*, apenas para registo.

## Como citar

Santos, E. J. dos (2026). *Impacto dos Sistemas de Certificação de Qualidade nos Pequenos Produtores Agrícolas na Agricultura Sustentável em Portugal* [Dissertação de Mestrado, Universidade do Algarve]. Apêndices disponíveis em https://github.com/Ervas1989/dissertacao-mestrado-apendices

---

© 2026 Ervaldo Jorge dos Santos. Todos os direitos reservados.
