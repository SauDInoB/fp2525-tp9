# AGENTS.md

Quarto website para a UC Ferramentas de Produtividade (FP 2025/2026), TP9. Conteúdo educativo em português europeu.

## Build

```bash
quarto render
```

O output vai para `docs/` (fonte do GitHub Pages). Site publicado em SauDInoB.github.io/fp2526-tp9.

## Ficheiros principais

| Ficheiro | Função |
|---|---|
| `index.qmd` | Slides revealjs da aula (2 horas) |
| `guiao.qmd` | Guião passo-a-passo com código copy-paste |
| `exercicio.qmd` | Enunciado do trabalho (componente E3) |
| `_quarto.yml` | Configuração do site (navbar, output-dir) |
| `custom.css` | Estilos partilhados (slides e páginas HTML) |
| `ficheiros/` | Datasets CSV para download pelos grupos |
| `scripts/gerar_datasets.R` | Gera os quatro CSVs com seed fixo |

## Regras para edição

- Português europeu exclusivamente. Nunca: usuário, arquivo, tela. Sempre: utilizador, ficheiro, ecrã.
- Sem emojis em nenhum ficheiro.
- Sem bold nem itálico excessivo. Usar apenas onde tecnicamente necessário.
- Sem travessões longos. Usar vírgulas, parênteses ou dois pontos.
- Código R segue as convenções de https://tiagojct.eu/notes/rst-bp/: projectos RStudio, `here::here()`, pipe nativo `|>`, sections com `# ----`, `library()` no script, `install.packages()` nunca no script.
- `custom.css` é imutável: paleta IBM Plex + cores definidas em `:root`.
- Deploy automático via GitHub Actions para branch `gh-pages`.
- Os datasets têm seed fixo (`set.seed(20260421)`). Qualquer regeneração mantém a semente.

## Notas de workflow

- `docs/` está no `.gitignore`: nunca fazer commit do output renderizado.
- Todos os `.qmd` usam `custom.css`.
- Os slides usam formato `revealjs` com tema `simple`.
- As páginas HTML usam tema `cosmo`.

## Adicionar conteúdo

- Novos slides: adicionar a `index.qmd` com `# Secção {background-color="#2C3E50"}` para section headers.
- Novas páginas: adicionar ao navbar em `_quarto.yml` e criar o ficheiro `.qmd` correspondente.
- Novos ficheiros de exercício: colocar em `ficheiros/` (seguido em git).
