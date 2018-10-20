# Minicurso de Programação COBOL


## Sumário
> * [Introdução](#introducao)
> * [Regras de um programa COBOL](#regras-de-um-programa-cobol)
>   * [Regras de codificação](#regras-de-codificacao)
>   * [Divisões](divisões)
> * [Palavras Reservadas](#palavras-reservadas)
>   * [Constantes Figurativas](#constantes-figurativas)
>   * [Condições de Classe](#condições-de-classe)
> * [Variáveis](#variáveis)
>   * [Definição de dados](#definição-de-dados)
>   * [Níveis de dados](#níveis-de-dados)
> * [Comando Aritméticos](#comando-aritméticos)
> * [Outros comandos básicos](#outros-comandos-básicos)


## Introdução
Este repositório tem como objetivo reunir códigos e conceitos básicos da linguagem de programação COBOL (COmmon Bussines Oriented Language), que será ministrada como minicurso na 2ª Edição do FlashCLIP. 

 ####  Palestrantes: 
- [Lucas Costa](https://github.com/olucascosta)
- [Pablo Henrique](https://github.com/pablohenriques)
- [Thiago Aparecido](https://github.com/thiagoaparecid97)

Para executar programas COBOL é necessário que você tenha um interpretador da linguagem instalado na sua máquina, no curso utilizaremos software OpenCobolIDE que pode ser obtido através deste link (https://launchpad.net/cobcide/+download) baixe e instale-o normalmente no seu computador. 

## Regras de um programa COBOL

### Regras de codifição

O programa de origem do COBOL deve ser escrito em um formato aceitável para os compiladores. COBOL programas são escritos em COBOL folhas de codificação. Há 80 caracteres posição sobre cada linha de uma codificação folha.

![regras-de-codificacao](https://user-images.githubusercontent.com/35963197/47170938-d05a8a00-d2dd-11e8-9f4f-a89310d854c6.png)


| Colunas | Definição 
|:-------:|---------------------------------------------------------------------------------------------------|
|  1 a 6  | Reservado para números de linha.                                                                  |
|    7    | Ele pode ter asterisco (*) indicando comentários e hífen (-) continuação de literal não numérico. |
|  8 a 11 | Aqui vão as divisões, seções e parágrafos.                                                        |
| 12 a 72 | Os comandos propriamente ditos devem ficar nessa área. |

Pode-se digitar até a coluna 80 mas tudo que for digitado entre 73 e 80, será considerado como um comentário. Atualmente, em muitos compiladores, essas regras não são mais necessárias. Mas é importante conhecer essas regras, pois dependendo da versão da sua ferramenta COBOL, poderá se deparar com programas utilizando essa estrutura.

### Divisões

Um programa COBOL possui 4 DIVISIONS (divisões), que por sua vez possuem SECTIONS (seções). Cada divisão/seção tem um objetivo bem específico, o que torna o programa muito legível e fácil de entender, pois cada definição fica em um local específico.  

```cobol
IDENTIFICATION DIVISION. (divisão de identificação)
ENVIRONMENT DIVISION. (divisão de ambiente)
DATA DIVISION. (divisão de dados)
PROCEDURE DIVISION. (divisão de procedimentos)
```
#### IDENTIFICATION DIVISION

É a divisão de identificação do programana na qual se fornece informações sobre o programa, tais como nome do programa, autor, comentários do autor e informações de uso para o usuário final. Essa divisão não possui seções. Esta divisão possui a seguinte estrutura:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. nome-programa.
AUTHOR. nome-autor.
INSTALLATION. local-desenvolvimento.
DATE-WRITTEN. mm/dd/yy.
DATE-COMPILED. mm/dd/yy. HH:MM:SS.
```

- PROGRAM-ID. Onde deverá ser informado o nome do programa a ser feito.
- AUTHOR. (opcional) Onde poderá ser indicado o nome do autor do programa
- INSTALLATION. (opcional) Onde poderá conter o nome da empresa ou local de geração do programa.
- DATE-WRITTEN. (opcional) Onde poderá conter a data em que o programa foi codificado.
- DATE-COMPILED. (opcional) Onde poderá conter a data em que o programa foi compilado.
- SECURITY. (opcional) Onde poderá conter os comentários sobre a segurança do programa e/ou seus arquivos.
- REMARKS. (opcional) Onde poderá conter os comentários adicionais sobre o programa.

Todos os comandos opcionais não possuem nenhum efeito na aplicação, são apenas parâmetros para documentação do programa, mas, caso queira utilizar algum deve ser escrito corretamente para que não cause erro de compilação.

#### ENVIROMENT DIVISION

É a divisão de ambiente, descreve o computador e os periféricos que serão utilizados pelo programa, fazendo ligação com o ambiente operacional onde o programa será executado, pode ser dividida em duas seções:

```cobol
ENVIRONMENT DIVISION. 
CONFIGURATION SECTION. 
SOURCE-COMPUTER. nome.
OBJECT-COMPUTER. nome.
SPECIAL-NAMES.   (comandos).
INPUT-OUTPUT SECTION.
FILE-CONTROL.    (comandos).
I-O-CONTROL.     (comandos).
```

- CONFIGURATION SECTION. Destina-se a configuração do ambiente
    - SOURCE-COMPUTER. Identifica em qual computador o programa foi feito.
    
    - OBJECT-COMPUTER. Identifica o computador no qual o programa está.
    
    - SPECIAL-NAMES. É onde se especifica comandos que atribuem características como sinal monetário, tipo de ponto decimal e caracteres simbólicos necessários para o bom funcionamento da aplicação.

- INPUT-OUTPUT SECTION. Esta seção destina-se a configuração do ambiente de Leitura e Gravação
    - FILE-CONTROL. Destina-se a especificação dos arquivos que o programa irá acessar.
    
    - I-O CONTROL. É onde é especificado como o programa deve receber as informações por outros meios de entrada, como cartões perfurados.


#### DATA DIVISION
Essa é a divisão de dados, na qual se declaram as variáveis, as constantes e todos os tipos de dado que irão alocar memória no decorrer do processo, pode ser dividida por essas seções:

```cobol
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
SCREEN SECTION.
```

- FILE SECTION. Aqui são definidas todas as variáveis e constantes utilizadas pelo programa e que realmente serão gravadas em arquivo.
- WORKING-STORAGE SECTION. Aqui são definidas todas as variáveis e constantes utilizadas pelo programa que serão apenas utilizadas em tempo de execução (tais como variáveis utilizadas para armazenar valores temporários e operadores lógicos).
- LINKAGE-STORAGE SECTION. Descreve os parâmetros formais de entrada e de retorno de dados
- SCREEN SECTION. Descreve as telas a serem exibidas durante a execução do programa


#### PROCEDURE DIVISION

Esta é a divisão de procedimentos, contém o código que irá manipular os dados descritos na DATA DIVISION. É nesta divisão que o desenvolvedor descreverá a lógica do programa. Consiste em instruções executáveis ​​usando variáveis ​​definidas na divisão de dados. Nesta divisão, nomes de parágrafo e seção são definidos pelo usuário. Deve haver pelo menos uma declaração na divisão de procedimento. A última instrução para encerrar a execução nesta divisão é STOP RUN que é usado nos programas de chamada ou EXIT PROGRAM que é usado nos programas chamados.

## Palavras Reservadas

Existem muitas palavras reservadas em COBOL, normalmente referente a funções pré-estabelecidas (a linguagem não permite a escrita de função). Na parte da leitura é útil pois os nomes descrevem o que a função faz (ex: SUM é a função para somar), já para escrita é um pouco menos usual porque é necessário saber como usar as palavras reservadas. Você pode visualizar todas palavras reservadas do COBOL acessando esse [link](https://www.ibm.com/support/knowledgecenter/pt-br/SSZJPZ_9.1.0/com.ibm.swg.im.iis.ds.mfjob.dev.doc/topics/r_dmnjbref_COBOL_Reserved_Words.html).

### Constantes Figurativas 

São literais geradas pelo compilador e usados como palavras reservadas. Algumas dessas palavras são descritas abaixo. Pode-se usar tanto o plural quanto o singular.

**ZERO, ZEROS, ZEROES** Representa o valor numérico "zero" ou uma ou mais ocorrências do caracter 0. 

**SPACE, SPACES** Representa um ou mais espaços. 

**HIGH-VALUE, HIGH-VALUES** Representa um ou mais caracteres com valores-altos. Usualmente é o  hexadecimal  "FF". 

**LOW-VALUE, LOW-VALUES** Representa um ou mais caracteres com valores-baixos. Usualmente é o  binário 0. 

**ALL Literal** Representa um conjunto de caracteres pré definido. 

**NULL, NULLS** Representa o valor numérico  "zero". Também representa um endereço inválido de  memória quando usado em conjunto com tipos de dados POINTER. igurative constants for a literal  restricted to numeric literals. 
 
### Condições de Classe 

A condição de classe é usada para testes onde se deseja saber se uma variável é formada ou não de um tipo particular de dados.

**NUMERIC** Numerico, caracteres de 0 a 9.  
**ALPHABETIC** Alfabético, caracteres de A - Z, de a – z e espaços.  
**ALPHABETIC-UPPER** Alfabético, caracteres de A - Z, e espaços.  
**ALPHABETIC-LOWER** Alfabético, caracteres de a - z, e espaços.

Exemplo:
```cobol
IF NOME IS NOT ALPHABETIC-UPPER
    DISPLAY “INFORME APENAS DE A – Z E ESPAÇOS” 
END-IF
```

## Variáveis

No COBOL há três tipos básicos de dados usados nas instruções, numérico (sinalizado ou não, com decimal ou inteiros), alfabética e alfanumérico.

Todas as variáveis e constantes utilizadas devem ser declaradas dentro da **DATA DIVISION** e nas seções corretas, de acordo com as suas finalidades no programa.

### Definição de dados  
As regras para nomes de variáveis são: 
- No máximo 30 caracteres 
- Não pode conter espaços ou brancos
- Pode começar com um número, mas precisa ter pelo menos um caractere alfabético
- Não podem ser utilizadas palavras reservadas da linguagem
- Pode conter apenas os caracteres de A até Z, de 0 até 9 e também “-” e “_”, e nenhum outro tipo de caracter é permitido. 

### Níveis de dados

Em COBOL, é possível hierarquizar as variáveis, a fim de facilitar a vida dos usuários do sistema. Os números de nível vão de 01 a 49 e os números 77, 78 e 88 são níveis especiais que permitem um uso diferenciado para as variáveis.

Os tipos de dados são definidos através da cláusula PICTURE, ou simplesmente PIC.

Exemplo:
```cobol
WORKING-STORAGE SECTION.
77 SALARIO PIC 9(007)V99.
77 NOME PIC X(030).
77 SALDO PIC S9(010)V99.
01 DATA-DE-HOJE.
    05 ANO PIC 9(004).
    05 MES PIC 9(002).
    05 DIA PIC 9(002).
```

#### Nível 77
Nível 77 é utilizado para declarar variáveis que não irão possuir sub-itens, isso significa que a variável não tem relacionamento definido com nenhuma outra variável.
#### Nível 78
Nível 78 é usado para definir constantes, que são valores imutáveis, por isso não precisam ser definidos em variáveis, normalmente são utilizados para facilitar a codificação, dando nomes significativos a
valores.

```cobol
78 PI VALUE 3.14159.
78 AZUL VALUE 1.
78 BRANCO VALUE 7.
```

#### Nível 88
Nível 88 é utilizado para definição de condição de variáveis, sua utilização é opcional, mas costuma tornar o código muito mais legível.

```cobol
77 HORA PIC 9(002).
    88 HORA-OK VALUES 0 THRU 23.
IF NOT HORA-OK
    DISPLAY “HORA INVALIDA !!”
END-IF
```
Sem Nível 88
```cobol
77 HORA PIC 9(002).
IF HORA > 23
    DISPLAY “HORA INVALIDA !!”
END-IF
```

## Comando Aritméticos


| Símbolo | Operador     | Descrição     |
|---------|--------------|---------------|
|    +    | [ADD](#add)        | Adição        |
|    -    | [SUBTRACT](#subtract)   | Subtração     |
|    *    |[MULTIPLY](#multiply)     | Multiplicação |
|    /    | [DIVIDE](#divide)    | Divisão       |
|    **   | *Não possui* | Exponenciação |

No caso da Exponenciação não há um operador, assim para realizar o cálculo com o símbolo é necessário utilizar o comando [COMPUTE](#compute).

### ADD
Utilizado para adicionar um valor a uma variável.

1ª Sintaxe:  
```cobol
ADD [nome-de-dado-1], [nome-de-dado-2] TO (nome-de-dado-n);
```
2ª Sintaxe:
```cobol
ADD [nome-de-dado-1], [nome-de-dado-2] GIVING (nome-de-dado-n);
```

Regras:
- O comando **ADD** tem a função de somar dois ou mais valores numéricos e armazenar a soma resultante;
- Quando a opção **TO** é usada, os valores de todos os nomes-de-dados, incluindo nome-de-dado-n, são somados e o resultado é armazenado em nome-de-dado-n;
- Quando a opção **GIVING** é usada pelo menos dois nomes-de-dados e/ou literais-numéricos devem seguir a palavra ADD.

Exemplos:    

`
ADD A TO B 
`   
*B = A + B* - Soma o valor de “A” e “B” e armazena o resultado em “B”

`
ADD A B TO C
`   
*C = A + B + C* - Soma o valores de “A”, “B” e “C” e armazena o resultado em “C”

`
ADD A B GIVING RESULTADO
`   
*RESULTADO = A + B* - Soma o valores de “A” e “B” e armazena o resultado em “RESULTADO”

### SUBTRACT
Utilizado para subtração de valores.

1ª Sintaxe:  
```cobol
SUBTRACT [nome-de-dado-1], [nome-de-dado-2] FROM (nome-de-dado-n);
```
2ª Sintaxe:  
```cobol
SUBTRACT [nome-de-dado-1], [nome-de-dado-2] FROM [nome-de-dado-n-1] GIVING (nome-de-dado-n)
```

Regras:
- No comando **SUBTRACT** os nome-de-dados que precedem **FROM** são somados e esta soma é subtraida do nome-de-dado que sucede **FROM**;

- O resultado será armazenado em nome-de-dado-n se houver **GIVING** e, em nome-de-dado-m se houver.

Exemplos:

`
SUBTRACT A B FROM C
`   
*C = C – (A + B)*


`
SUBTRACT A B 2 FROM C
`   
*C = C – (A + B + 2)*

`
SUBTRACT A B 2 FROM 200 GIVING C
`   
*C = 200 – (A + B + 2)*

### MULTIPLY
Utilizado para multiplicação de valores.

1ª Sintaxe:  
```cobol
MULTIPLY [nome-de-dado-1] BY (nome-de-dado-2);
```

2ª Sintaxe:  
```cobol
MULTIPLY [nome-de-dado-1] BY [nome-de-dado-2] GIVING (nome-de-dado-n)
```

Regras:
- Calcula o produto de dois itens-de-dados numéricos e armazena o resultado.
- Quando a opção **GIVING** é usada, o produto vai para nome-de-dado-n e quando não, o produto vai para o nome-de-dado-2.


Exemplos:

`
MULTIPLY A BY B
`   
*B = A X B*

`
MULTIPLY A BY 5 GIVING C
`   
*C = A X 5*

### DIVIDE
Utilizado para divisão de valores. O comando *DIVIDE* divide dois valores numéricos e armazena o quociente.

Sintaxe:  
```cobol
DIVIDE {nome-de-dado-1} {BY/INTO} {nome-de-dado-2} GIVING {nome-de-dado-3}
```
Regras:

- A instrução **DIVIDE** divide um item numérico por outro armazenando o resultado no item especificado.
- A forma **BY** diz que o primeiro operando é o dividendo, e que o segundo operando é o divisor;
- Para a forma **INTO**, vale o contrário. Se a opção **GIVING** não estiver presente, o operando que representar o dividendo deve ser um nome-de-dado que armazenará o quociente. A divisão por zero sempre cria um condição de erro.


Exemplos:

`
DIVIDE A INTO B
`   
*B = B / A*

`
DIVIDE A INTO B GIVING C REMAIDER D.
`   
*C = B / A*

`
DIVIDE A BY B GIVING C REMAIDER D.
`   
*C = A / B*

OBS:
1) D armazena o resto da divisão.
2) Só se usa *BY* com o comando *GIVING*.

### COMPUTE

O COMPUTE é utilizado para cálculos formados por expressões matemáticas de diferentes complexidades, podendo-se utilizar parênteses para priorizar cálculos e organizar a expressão.

O operadores são: '+' para adição, '-' para subtração,'*' para multiplicação, '/' para divisão e '**' para exponenciação.

Sintaxe:  
```cobol
COMPUTE [nome-de-dado-1] = nome-de-dado-2 [simbolos] nome-de-dado-n
```

Exemplos:

`
COMPUTE F = A + B / ( ( C – D ) * E )
`   

Quando se usa parenteses valem as seguintes regras:
- Um abre parenteses é precedido por um ou mais espaços;
- Um fecha parenteses é seguido por um ou mais espaços;

## Outros comandos básicos
**DISPLAY**: É responsável pela exibição de texto para o usuário do sistema.    
**ACCEPT X FROM Y**: É responsável pela entrada de dados para uma variável.    
**MOVE X TO Y**: É o comando que atribui valores a uma variável.    
**GO TO/PERFORM**: Executa um trecho específico do programa.
