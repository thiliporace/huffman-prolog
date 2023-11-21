% Thiago Leandro Liporace, 42128481
% Caue Macedo de Souza, 42144914

% Huffman em Prolog


:- dynamic frequency_table/2, huffman_tree/1.

% Arvore de Huffman
:- use_module(library(pairs)).
:- use_module(library(heaps)).

% Le arquivo e cria tabela
read_input_file(File) :- open(File, read, Stream),
    process_input_lines(Stream),
    close(Stream).

process_input_lines(Stream) :-
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  process_input_codes(Codes),
        process_input_lines(Stream)
    ;   true
    ).

process_input_codes([]).
process_input_codes([Char|Rest]) :-
    process_input_char(Char),
    process_input_codes(Rest).

process_input_char(Char) :-
    is_valid_character(Char) ->
        atom_codes(Atom, [Char]),
        update_frequency_table(Atom);
    true.

is_valid_character(Char) :-
    char_type(Char, alpha) ; char_type(Char, digit) ; member(Char, [46, 44, 59, 33, 63, 58, 40, 41, 45, 95, 43, 61, 64, 35, 36, 37, 38, 42, 47, 92, 124, 60, 62, 34, 39, 94, 126, 96]).

update_frequency_table(Char) :-
    (frequency_table(Char, Freq) ->
        NewFreq is Freq + 1,
        retract(frequency_table(Char, _)),
        assertz(frequency_table(Char, NewFreq))
    ; assertz(frequency_table(Char, 1))
    ).

% Imprime a tabela de frequencias
print_frequency_table :-
    tell('output.txt'),  % Redirecionar a saída para o arquivo 'output.txt'
    write('Tabela de Frequências:\n'),
    write('------------------------\n'),
    forall(frequency_table(Char, Freq), print_frequency_entry(Char, Freq)),
    told.  % Fechar o arquivo de saída

print_frequency_entry(Char, Freq) :-
    format('~w\t\t~d\n', [Char, Freq]).

% Constroi a arvore
construct_huffman_tree :-
    findall(Freq-leaf(Char), frequency_table(Char, Freq), Pairs),
    list_to_heap(Pairs, Heap),
    construct_huffman_tree_helper(Heap, Tree),
    assertz(huffman_tree(Tree)).  % Armazenar a árvore construída

construct_huffman_tree_helper(Heap, Tree) :-
    get_from_heap(Heap, Freq1, Tree1, Heap1),
    (   get_from_heap(Heap1, Freq2, Tree2, Heap2)
    ->  NewFreq is Freq1 + Freq2,
        NewTree = node(Tree1, Tree2),
        add_to_heap(Heap2, NewFreq, NewTree, NewHeap),
        construct_huffman_tree_helper(NewHeap, Tree)
    ;   Tree = Tree1  % Quando apenas um elemento restar no heap
    ).

% Gera codigo de huffman
generate_codes_from_huffman_tree(Tree, Codes) :-
    generate_codes_helper(Tree, [], Codes).

generate_codes_helper(leaf(Char), Code, [Char-Code]).
generate_codes_helper(node(Left, Right), Prefix, Codes) :-
    generate_codes_helper(Left, [0|Prefix], LeftCodes),
    generate_codes_helper(Right, [1|Prefix], RightCodes),
    append(LeftCodes, RightCodes, Codes).

% Converte lista em string
convert_digits_to_string(Digits, String) :-
    maplist(atom_number, Atoms, Digits),  % Converte cada dígito em átomo
    atomic_list_concat(Atoms, String).     % Concatena os átomos em uma string

% Imprime tabela
print_table_with_huffman_codes :-
    huffman_tree(Tree),
    generate_codes_from_huffman_tree(Tree, Codes),
    tell('output.txt'),
    write('Caractere   Frequência   Código Huffman\n'),
    write('----------------------------------------\n'),
    forall((frequency_table(Char, Freq), member(Char-CodeList, Codes)),
           (convert_digits_to_string(CodeList, CodeString),
            format('~w\t\t~d\t\t~s\n', [Char, Freq, CodeString]))),
    told.



main :-
    read_input_file('input.txt'), construct_huffman_tree,
    print_table_with_huffman_codes.
