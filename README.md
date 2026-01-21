# 🐍 Zython - Interpretador Python Escrito em Zig

> **Zython** é uma implementação leve e educacional de um interpretador para a linguagem Python, construído inteiramente utilizando **Zig**.

O projeto visa explorar os fundamentos da construção de linguagens de programação (Lexing, Parsing, ASTs, Interpretação) aproveitando o sistema de tipos robusto, o controle de memória manual e a performance moderna que o Zig oferece.

## 🚀 Por que usar (e estudar) o Zython?

Embora o CPython seja a referência, o Zython oferece uma perspectiva única:

1.  **Aprendizado de Zig na Prática**: Veja como usar *Tagged Unions*, *Allocators* (como ArenaAllocator) e o sistema de tratamento de erros do Zig em uma aplicação real e complexa.
2.  **Entendimento Interno de Linguagens**: O código é desenhado para ser legível. Você pode entender exatamente como:
    *   A indentação do Python (espaços em branco significativos) é transformada em tokens `INDENT` e `DEDENT`.
    *   A precedência de operadores matemáticos é resolvida no Parser.
    *   Como escopos de variáveis funcionam "por baixo do capô".
3.  **Performance e Controle**: Diferente de interpretadores escritos em linguagens com Garbage Collection (como Java ou o próprio Python), aqui gerenciamos a memória explicitamente, permitindo otimizações agressivas e zero overhead de runtime oculto.
4.  **Base para DSLs**: Se você precisa de uma linguagem de script parecida com Python para embedar em seu projeto Zig, o Zython serve como um excelente ponto de partida minimalista.

## ✨ Funcionalidades Atuais (MVP)

*   **Tipos de Dados**: Números (`int`/`float`), Strings, Booleanos e `None`.
*   **Aritmética**: Operações completas (`+`, `-`, `*`, `/`) com precedência correta.
*   **Lógica**: Comparadores (`==`, `!=`, `>`, `<`, `>=`, `<=`).
*   **Variáveis**: Declaração implícita e uso de variáveis.
*   **IO**: Função `print()` nativa.
*   **Sintaxe Pythonica**: Suporte real a indentação significativa.
*   **REPL**: Shell interativo para experimentação rápida.

## 🛠️ Instalação e Uso

### Pré-requisitos
*   **Zig**: Versão `0.14.0` ou superior (testado na `0.15.0-dev`).

### Compilando e Rodando

1.  **Clone o repositório:**
    ```bash
    git clone https://github.com/seu-usuario/zython.git
    cd zython
    ```

2.  **Execute um script de exemplo:**
    ```bash
    zig build run -- examples/hello.py
    ```

3.  **Inicie o modo REPL (Interativo):**
    ```bash
    zig build run
    ```

## 🗺️ Roadmap

*   [x] Lexer e Parser básicos
*   [x] Expressões Aritméticas e Variáveis
*   [ ] **Fase 2**: Controle de Fluxo (`if`, `else`, `while`)
*   [ ] **Fase 3**: Funções (`def`) e Recursão
*   [ ] **Fase 4**: Listas e Dicionários
*   [ ] **Fase 5**: Módulos e Imports

## 📄 Licença

Este projeto é distribuído sob a licença MIT. Sinta-se livre para usar, estudar e modificar.
