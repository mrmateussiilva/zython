pub const TokenType = enum {
    // Identificadores e Literais
    Identifier,
    String,
    FString,
    Number, // Int or Float for simplicity in lexer, refined in parser

    // Palavras-chave
    And, Class, Else, False, For, Fun, If, Import, In, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    // Operadores e Pontuação
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    PlusEqual, MinusEqual, StarEqual, SlashEqual,
    Colon,

    // Especiais Python
    Newline,
    Indent,
    Dedent,

    // Fim de arquivo
    Eof,
    
    // Erro léxico
    Error,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    col: usize,
};
