# 🗺️ Zython Roadmap

Este documento lista as funcionalidades planejadas para transformar o Zython em uma alternativa robusta para scripting.

## 🚀 Fase 1: Compatibilidade da Linguagem
Funcionalidades sintáticas para suportar código Python idiomático moderno.

- [ ] **Context Managers (`with`)**: Essencial para manipulação segura de arquivos (`with open(...) as f:`).
- [ ] **List Comprehensions**: Sintaxe concisa para criar listas (`[x*2 for x in lista if x > 0]`).
- [ ] **Argumentos Variáveis**: Suporte a `*args` e `**kwargs` em funções.
- [ ] **Decorators**: Sintaxe `@wrapper` para funções e classes.
- [ ] **Slices Avançados**: Suporte completo a fatiamento de listas e strings (`lista[start:stop:step]`).
- [ ] **Operadores Unários**: Suporte a `+x`, `~x`.
- [ ] **Assert**: Declaração `assert condition`.

## 📦 Fase 2: Biblioteca Padrão (Built-ins)
Implementação de módulos essenciais em Zig para dar "baterias" à linguagem.

- [ ] **sys**: Acesso a argumentos de linha de comando (`argv`), saída (`exit`), stdin/stdout/stderr.
- [ ] **math**: Funções matemáticas comuns (`sin`, `cos`, `sqrt`, `pow`, constantes `pi`, `e`).
- [ ] **os**: Interação com sistema operacional (variáveis de ambiente, listar diretórios, verificar arquivos).
- [ ] **time**: Funções de tempo (`time()`, `sleep()`).
- [ ] **json**: Parser e serializer JSON básico.

## 🏗️ Fase 3: Arquitetura e Performance
Melhorias profundas no núcleo do interpretador.

- [ ] **Garbage Collector (GC)**: Substituir o `ArenaAllocator` (que libera memória apenas no final) por um GC real (Mark-and-Sweep ou Reference Counting) para permitir execução de longa duração.
- [x] **Bytecode Compiler & VM**: Migrar de interpretador Tree-Walk (AST) para uma Máquina Virtual de Bytecode para ganho significativo de performance.
- [ ] **REPL Melhorado**: Histórico de comandos, auto-complete, multiline editing.

## 🔌 Fase 4: Interoperabilidade
- [ ] **FFI (Foreign Function Interface)**: Capacidade de carregar bibliotecas dinâmicas (`.so`/`.dll`) e chamar funções C.
- [ ] **Zig Interop**: Facilidade para estender o Zython usando Zig.
