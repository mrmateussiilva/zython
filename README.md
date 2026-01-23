# 🐍 Zython - Interpretador Python Escrito em Zig

> **Zython** é uma implementação leve e educacional de um interpretador para a linguagem Python, construído inteiramente utilizando **Zig**.

O projeto explora os fundamentos da construção de linguagens (Lexing, Parsing, ASTs, Interpretação) com foco em performance e gerenciamento de memória manual.

## ✨ Funcionalidades Implementadas (Status Atual)

O interpretador já suporta um subconjunto rico da linguagem:

*   **Orientação a Objetos**:
    *   Definição de Classes (`class Nome:`).
    *   Métodos e Construtor (`__init__`).
    *   Instanciação de Objetos.
    *   Acesso e modificação de propriedades (`obj.prop = valor`).
    *   Uso de `self` (this) dentro de métodos.
*   **Funções e Escopo**:
    *   Funções de primeira classe (`def`).
    *   **Closures** (funções capturam variáveis do escopo onde foram criadas).
    *   Escopos locais e globais.
*   **Controle de Fluxo**:
    *   `if`, `else`.
    *   `while`.
    *   Laços `for`: `for item in lista:`.
*   **Estruturas de Dados**:
    *   Listas/Arrays: `x = [1, 2, 3]`.
    *   Dicionários/Maps: `d = {'a': 1, 2: 'b'}`.
    *   Indexação e modificação: `x[0] = 10`, `d['a'] = 20`.
    *   Métodos nativos: `.append()`, `len()`.
    *   Arquivos: `open()`, `read()`, `write()`, `close()`.
    *   Strings: `.split()`, `.strip()`.
    *   Números (`float64`).
    *   Strings (com concatenação `+`).
    *   Booleanos (`True`, `False`).
    *   `None`.

## 🚧 O Que Falta (Roadmap para Funcionalidade Completa)

Para o Zython ser útil em scripts reais, as seguintes funcionalidades são prioritárias:

1.  **Iteração e Auxiliares**:
    *   [ ] Funções auxiliares: `range()`.
2.  **Refinamento OOP**:
    *   [ ] Herança simples: `class Filho(Pai):`.
    *   [ ] `super()`.
4.  **Sistema de Módulos**:
    *   [ ] `import` de outros arquivos `.py`.

## 🛠️ Instalação e Uso

### Pré-requisitos
*   **Zig**: Versão `0.14.0` ou superior.

### Rodando Exemplos

1.  **Script Simples**:
    ```bash
    zig build run -- examples/hello.py
    ```

2.  **Testando Classes**:
    ```bash
    zig build run -- tests/class_test.py
    ```

3.  **Modo Interativo (REPL)**:
    ```bash
    zig build run
    ```