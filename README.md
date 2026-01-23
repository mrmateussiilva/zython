# 🐍 Zython - Interpretador Python Escrito em Zig

> **Zython** é uma implementação leve e educacional de um interpretador para a linguagem Python, construído inteiramente utilizando **Zig**.

O projeto explora os fundamentos da construção de linguagens (Lexing, Parsing, ASTs, Interpretação e Bytecode/VM) com foco em performance e gerenciamento de memória manual.

## ✨ Funcionalidades Implementadas (Status Atual)

O interpretador já suporta um subconjunto rico da linguagem (via VM de bytecode com fallback para tree-walker quando algo ainda não é suportado):

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
    *   Slices: `x[1:3]`, `x[:2]`, `x[2:]`.
    *   Métodos nativos: `.append()`, `len()`.
    *   Métodos adicionais: `.pop()`, `.extend()`, `.get()`, `.lower()`, `.upper()`.
    *   Arquivos: `open()`, `read()`, `write()`, `close()`.
    *   Strings: `.split()`, `.strip()`.
    *   Números (`float64`).
    *   Strings (com concatenação `+`).
    *   Booleanos (`True`, `False`).
    *   `None`.
*   **Modularidade**:
    *   Sistema de Módulos: `import` de outros arquivos `.py`.

## 🚧 O Que Falta (Roadmap para Funcionalidade Completa)

Para o Zython ser útil em scripts reais, as seguintes funcionalidades são prioritárias:

1.  **Iteração e Auxiliares**:
    *   [ ] Funções auxiliares: `range()`.
2.  **Refinamento OOP**:
    *   [ ] Herança simples: `class Filho(Pai):`.
    *   [ ] `super()`.

## 🛠️ Instalação e Uso

### Pré-requisitos
*   **Zig**: Versão `0.14.0` ou superior.

### Rodando Exemplos

1.  **Script Simples**:
    ```bash
    zig build run -- examples/hello.py
    ```
    
1.  **Executar em ReleaseFast**:
    ```bash
    zig build run-release -- examples/hello.py
    ```

1.  **Slices e Métodos**:
    ```bash
    zig build run-release -- examples/slice_methods.py
    ```

2.  **Testando Classes**:
    ```bash
    zig build run -- tests/class_test.py
    ```

3.  **Testando Importação de Módulos**:
    *   Crie um arquivo `mylib.py` no diretório raiz do projeto com o conteúdo:
        ```python
        # mylib.py
        value = 123
        def greet():
            print("Hello from mylib!")
        ```
    *   Crie um arquivo `main.py` no diretório raiz do projeto com o conteúdo:
        ```python
        # main.py
        import mylib
        print(mylib.value)
        mylib.greet()
        ```
    *   Execute o `main.py`:
        ```bash
        zig build run -- main.py
        ```

4.  **Modo Interativo (REPL)**:
    ```bash
    zig build run
    ```

### Observação sobre a VM
A VM de bytecode cobre os recursos mais usados (expressões, funções, listas, dicionários, imports e try/raise simples). Quando algo ainda não é suportado, o Zython recai automaticamente no tree‑walker.
