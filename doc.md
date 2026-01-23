# 📘 Documentação Técnica do Zython

Esta documentação detalha a arquitetura interna, as decisões de design e a especificação técnica do interpretador Zython.

---

## 1. Arquitetura do Sistema

O Zython segue a arquitetura clássica de um interpretador *Tree-Walk* (percorredor de árvore), mas agora possui uma VM de bytecode. O fluxo principal é:

```mermaid
Source Code (.py) -> [Lexer] -> Tokens -> [Parser] -> AST -> [Resolver] -> [Compiler] -> Bytecode -> [VM] -> Resultado
```

Quando um recurso ainda não está disponível na VM, o Zython faz fallback para o interpretador Tree‑Walk.

### 1.1 Gerenciamento de Memória (Arena Allocator)
Uma das maiores vantagens do Zython é o uso de **Arena Allocation**.
*   **Como funciona**: Em vez de alocar e liberar cada pequeno nó da AST individualmente (o que é lento e propenso a fragmentação), alocamos tudo em uma região contígua de memória (Arena).
*   **Benefício**: Quando o script termina (ou uma linha do REPL é processada), liberamos a Arena inteira de uma só vez. Isso torna o `deinit` extremamente rápido e elimina vazamentos de memória (memory leaks) na estrutura da AST.

---

## 2. Componentes Principais

### 2.1 Lexer (`src/lexer.zig`)
O Lexer é responsável por transformar texto bruto em uma sequência de `Tokens`.

**O Desafio da Indentação:**
Python define blocos de código através de indentação, não chaves `{}`. O Lexer do Zython gerencia isso mantendo uma **pilha (stack) de níveis de indentação**.
1.  Ao encontrar o início de uma linha, conta os espaços.
2.  Se `espaços > nível_atual`: Emite um token `INDENT` e empilha o novo nível.
3.  Se `espaços < nível_atual`: Desempilha níveis e emite tokens `DEDENT` até encontrar o nível correspondente.

### 2.2 Parser (`src/parser.zig`)
Utiliza a técnica **Recursive Descent Parsing** (Análise Descendente Recursiva).
*   Cada regra gramatical (expressão, declaração, comparação) tem sua própria função.
*   Define a precedência de operadores (ex: multiplicação acontece antes da adição) através da hierarquia de chamadas de função (`equality` -> `comparison` -> `term` -> `factor` -> `unary` -> `primary`).

### 2.3 AST (`src/ast.zig`)
A Árvore Sintática Abstrata é representada usando **Tagged Unions** do Zig. Isso permite uma representação de dados extremamente compacta e segura.

Exemplo da estrutura `Expr` (Expressão):
```zig
pub const Expr = union(enum) {
    Binary: struct { left: *Expr, op: BinaryOp, right: *Expr },
    Literal: Value,
    Variable: struct { name: []const u8, depth: i32, slot: i32 },
    // ...
};
```

### 2.4 Interpreter (`src/interpreter.zig`)
O cérebro da operação. Ele "caminha" pela AST gerada e executa as ações.
*   **Environment (Ambiente)**: Usa um `StringHashMap` para armazenar variáveis. Atualmente, suporta escopo global. No futuro, suportará escopos aninhados (local vs global) para funções.
*   **Sistema de Tipos (`Value`)**: O Zython é dinamicamente tipado. O `Value` é uma union que pode ser `Number`, `Boolean`, `String` ou `Nil`. O interpretador verifica os tipos em tempo de execução (Runtime Type Checking) antes de realizar operações.

### 2.5 VM de Bytecode (`src/compiler.zig`, `src/vm.zig`)
A VM executa bytecode compilado a partir da AST, reduzindo o overhead do tree‑walker.
*   **Compiler**: gera `Chunk` com opcodes e constantes.
*   **VM**: executa opcodes em uma pilha de `Value`.
*   **Resolver**: calcula profundidade e slots de variáveis locais antes da compilação.

---

## 3. Guia de Expansão

Para desenvolvedores que desejam adicionar funcionalidades ao Zython:

### Como adicionar um novo Operador (ex: Módulo `%`)

1.  **Token (`token.zig`)**: Adicione `Percent` ao enum `TokenType`.
2.  **Lexer (`lexer.zig`)**: No `switch`, reconheça o caractere `%` e retorne `.Percent`.
3.  **AST (`ast.zig`)**: Adicione `Mod` ao enum `BinaryOp`.
4.  **Parser (`parser.zig`)**: Atualize a função `factor` (onde residem multiplicação e divisão) para aceitar também `%`.
5.  **Interpreter (`interpreter.zig`)**: No `switch (b.op)`, adicione o caso `.Mod` e implemente a lógica matemática.

---

## 4. Aplicações e Casos de Uso

Embora seja um projeto educacional, a arquitetura do Zython permite aplicações práticas futuras:

1.  **Linguagem de Configuração**: Substituir arquivos JSON/YAML complexos por scripts Pythonicos que podem calcular valores dinamicamente.
2.  **Game Scripting**: Integrar em engines de jogos feitas em Zig para permitir que designers criem lógica de jogo sem recompilar o binário principal.
3.  **Automação**: Criar scripts de build ou automação que rodam nativamente sem depender de uma instalação do Python no sistema do usuário (uma vez que o Zython é um binário estático único).

---

## 5. Referência da Linguagem (Spec Atual)

### Variáveis e Tipos
```python
x = 10          # Number (f64)
nome = "Zython" # String
ativo = True    # Boolean
vazio = None    # Nil
```

### Operadores Suportados
*   Aritméticos: `+`, `-`, `*`, `/`
*   Comparação: `==`, `!=`, `>`, `<`, `>=`, `<=`

### Funções Built-in
*   `print(arg)`: Imprime o argumento no stdout e pula uma linha.
*   `len(obj)`: Retorna o tamanho de listas, strings e dicionários.

### Slices
*   Listas e strings suportam slices simples: `x[1:3]`, `x[:2]`, `x[2:]`, `x[-3:-1]`.

### Métodos de Lista
*   `.append(item)`, `.pop(index?)`, `.extend(list)`

### Métodos de String
*   `.split(delim)`, `.strip()`, `.lower()`, `.upper()`

### Métodos de Dicionário
*   `.get(key, default?)`
