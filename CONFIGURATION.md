# Configuration Format

The floskell configuration file is a `JSON` file.  The basic structure
is as follows:

```json
{
  "language": "Haskell2010",
  "extensions": [],
  "style": "base",
  "formatting": {
    "penalty": {...},
    "layout": {...},
    "indent": {...},
    "align": {...},
    "group": {...},
    "op": {...},
    "options": {...}
    }
  }
}
```

## Parser Settings

* `language` (`Haskell2010`): The Haskell language standard for parsing source files.
  Either `Haskell98` or `Haskell2010`.

  ```json
  {
    "language": "Haskell2010"
  }
  ```

* `extensions` (`[]`): A list of extensions to be enabled or disabled.  Check
  the output of `floskell --help` for the full list of extensions.  To
  disable an extension, prefix the extension with `No`.

  ```json
  {
    "extensions": ["RecordWildCards", "NoTypeFamilies"]
  }
  ```

* `fixities` (`[]`): A list of additional infix declarations to use
  while parsing.  Check the output of `floskell --help` for a list of
  built-in declarations.

  ```json
  {
    "fixities": ["infixr 9 ."]
  }
  ```

## Style Definition

* `style` (`base`): The formatting style to use.  The style can be fine-tuned
  using the `formatting` configuration element.  Check the output of
  `floskell --help` for the full list of styles.

* `formatting` (`{}`): Formatting settings, applied on top of the selected
  style.

### Line Penalty

* `penalty`: Defines the parameters for the penalty function used to
  judge the overall layout.

  * `indent` (`1`): Penalty for each indentation character.
  * `linebreak` (`10`): Penalty for each line.
  * `max-line-length` (`80`): Character limit for each line.
  * `overfull` (`10`): Penalty for each character over the line limit.
  * `overfull-once` (`200`): Penalty for each overfull line.

### Layout Rules

* `layout`: Defines the accepted layout choices for all configurable
  language constructs.

  Allowed values are `flex`, `vertical`, or `try-oneline`.

  * `app` (`flex`): Function application
  * `con-decls` (`flex`): Data constructor declarations
  * `constraints` (`flex`): Signature constraints
  * `declaration` (`flex`): Declarations
  * `export-spec-list` (`flex`): Module export list
  * `export-spec-inner-list` (`flex`): Module export list sublists
  * `if` (`flex`): If-then-else
  * `import-spec-list` (`flex`): Module import lists
  * `import-spec-inner-list` (`flex`): Module import lists sublists
  * `infix-app` (`flex`): Infix operator application
  * `let` (`flex`): Let-in
  * `list-comp` (`flex`): List comprehensions
  * `list` (`flex`): List
  * `record` (`flex`): Record declaration, construction, and update
  * `pattern-app` (`flex`): Pattern Applications
  * `pattern-synonymn` (`flex`): Pattern Synonymns
  * `type` (`flex`): Types in signatures (Specialized for several contexts) defaults to flex across all
  * `unboxed-sum` (`flex`): Unboxed sum

### Indentation Rules

* `indent`: Defines the indentation choices for all configurable
  language constructs.

  Allowed values (except for `onside`, `deriving`, `where`) are
  `align`, `indent-by n`, and `align-or-indent-by n`.

  Allowed values for `onside` are all integers greater than zero.

  Allowed values for `deriving` and `where` are all integers greater
  than or equal to zero.

  * `onside` (`4`): Onside indentation
  * `deriving` (`4`): Indentation for deriving lists
  * `where` (`2`): Indentation for where keyword after functions
  * `module-where` (`Nothing`): Indentation for module where keyword
  * `app` (`indent-by 4`): Indentation for application
  * `case` (`indent-by 4`): Case analysis
  * `class` (`indent-by 4`): Class and instance declarations
  * `do` (`indent-by 4`): Do and recursive do blocks
  * `export-spec-list` (`indent-by 4`): Module export list
  * `export-spec-inner-list` (`align`): Module export list inner lists
  * `if` (`indent-by 4`): If-then-else
  * `import-spec-list` (`indent-by 4`): Module import lists
  * `import-spec-inner-list` (`align`): Module import lists inner lists
  * `let` (`indent-by 4`): Let bindings
  * `let-binds` (`indent-by 4`): Let bindings
  * `let-in` (`indent-by 4`): Expression in let
  * `multi-if` (`indent-by 4`): Guards and multi-way if
  * `pattern-app` (`indent-by 4`): Pattern apps
  * `patternsig` (`indent-by 4`): Pattern signatures
  * `simple-declaration` (`align-or-indent-by 4`): Default for simple declarations, overridden in someplaces
  * `typesig` (`indent-by 4`): Type signatures
  * `where-binds` (`indent-by 2`): Bindings in where block after functions

### Tabstop Alignment

* `align`: Defines the enabled tabstop alignments and their limitations.

  Allowed values, except for `limits`, are `true` and `false`.

  The item `limits` must be a list with two integers, the first
  integer is a number of character (`absolute limit`), the second a
  percentage (`relative limit`).  Alignment will be disabled if the
  length of any inserted padding is larger than `absolute limit` and
  is larger than `relative limit` of the maximum field length (tabstop
  column minus indentation).

  * `limits` (`[10, 25]`): Alignment limits
  * `case` (`false`): Case patterns
  * `class` (`false`): Declarations inside class and instance declarations
  * `import-module` (`false`): Names of imported modules
  * `import-spec` (`false`): Module import lists
  * `import-as-min` (`Nothing`): Module "as" keyword min column
  * `let-binds` (`false`): Let bindings
  * `matches` (`false`): Function match clauses
  * `module-pragma-ends` (`false`): #-} in module pragmas
  * `multi-if-rhs` (`false`): rhs of multif and matches guards
  * `record-fields` (`false`): Record field assignments and type signatures
  * `where` (`false`): Declarations in where bindings for functions

### Whitespace Rules

* `group`: Defines whitespace rules for parentheses, brackets,
  braces, and other enclosing punctuation characters.

  The `group` item is a map from enclosing punctuation (identified
  by the opening characters) and optional context to a whitespace
  configuration. The key can be either

  Changes between here and original Floskell include the new decl_context for
  refining whitespace and layout, and when formatting vertically spaces will be
  obeyed using the following rule.
    * Before will place a space directly after the Before linebreak if set
    * After will place a space directly before the After linebreak if set

  * `default`: Defines the fallback whitespace configuration,

  * `* in <context>`: Defines the default whitespace configuration
    within the given context,

  * `<punct> in *`: Defines the default whitespace configuration for
    the given punctuation, or

  * `<punct> in <context>`: Defines the whitespace configuration for
    the given punctuation within the given context,

  * `* within <decl_context>`: Defines the specialized whitespace configuration
    within the given specialized decl_context,

  * `* in <context> within <decl_context>`: Defines the specialized whitespace configuration
    in a context within the given specialized decl_context,

  * `<punct> within *`: Defines the specialized whitespace configuration for
    the given punctuation, or

  * `<punct> within <context>`: Defines the specialized whitespace configuration for
    the given punctuation within the given decl_context,

  * `<punct> in * within *`: Defines the specialized whitespace configuration for
    the given punctuation, or

  * `<punct> in <context> within *`: Defines the specialized whitespace configuration for
    the given punctuation in a context, or

  * `<punct> in * within <decl_context>`: Defines the specialized whitespace configuration for
    the given punctuation in a decl_context, or

  * `<punct> in <context> within <decl_context>`: Defines the specialized whitespace configuration for
    the given punctuation in the given context within a decl_context ,

  where context is one of `declaration`, `type`, `pattern`,
  `expression`, or `other`.

  where decl_context is one of `module`, `record`, `gadt`, `gadt_field`,
  `gadt_field_type`, `type`, `comprehension`, `special`, `pattern`,
  `guard`, `export`, or `other`.

  The greatest power the above gives is in specializing overused syntax
  Most should be self explanatory , and combine with context to enhance formatting

  The value is an object with the following fields:

  * `spaces`: Where to insert spaces. In relation to the `op` or the `expression` in the group.
  * `linebreaks`: Where to insert linebreaks. `spaces` will be put after `before` linebreaks and before `after` libreaks if configured
  * `force-linebreak`: Whether to enforce vertical layout.
  * `spaces-h-override`: Due to the change making spaces operate in vertical contexts, this is an override used only in horizontal layout

  An example:

  ```haskell
  {
    "group": {
      "default": {
        "spaces": "none",
        "linebreaks": "after",
        "force-linebreak": false
        "spaces-h-override": "none"
      },
      "(": {
        "spaces": "both",
        "linebreaks": "after",
        "force-linebreak": false
        "spaces-h-override": "none"
      },
      ". within gadt_field": {
        "spaces": "after",
        "linebreaks": "before",
        "force-linebreak": true
        "spaces-h-override": "none"
      },
      ". within gadt_field": {
        "spaces": "both",
        "linebreaks": "none",
        "force-linebreak": false
        "spaces-h-override": "none"
      },
      "( in pattern": {
        "spaces": "none",
        "linebreaks": "none",
        "force-linebreak": false,
        "spaces-h-override": "both"
      }
    }
  }
  ```

* `op`: Defines whitespacce rules for infix operators.

  See `group` above.

### Other Style Options

* `options`: Defines additional formatting rules for module heads.

  * `align-sum-type-decl` (`false`): Whether to align `=` with `|` in the declaration of sum types.
  * `preserve-vertical-space` (`false`): Whether to preserve additional vertical space between declarations, statements, and a few other places.
  * `decl-no-blank-lines` (`[]`): Where to (not) insert blank lines between declarations (`module`, `class`, `instance`, `where`).
  * `sort-import-lists` (`false`): Whether to sort import statements by the name of the imported module.
  * `sort-import-spec-inner-lists` (`false`): Whether to sort inner list of import statements by the name of the imported symbol.
  * `sort-imports` (`false`): How to sort import lists (see below).
  * `sort-pragmas` (`false`): Whether to sort module pragmas (`LANGUAGE`, `OPTION`, and `ANN`).
  * `flexible-oneline` (`false`): Allow `do`, `mdo`, `case ... of`, `\case`, and `\... ->` in `try-oneline` layout.
  * `split-language-pragmas` (`false`): Whether to split `LANGUAGE` pragmas.
  * `multi-if-padding` (`false`): Whether to enable or disable vertical newline padding in multi-if and guards
  * `let-specialization` (`false`): Whether to apply some specialization to padding of let directly after rhs
      ``` haskell
      fn =
        -- Let is hung via specialization
        let x
        in y
      ```
  * `let-do-specialization` (`false`): Whether to apply some specialization to padding of do/mdo directly after let. in particular changes the "in" operator to be in special decl_context
  * `let-in-padding` (`[true, true]`): Whether to apply some specialization to padding of let and in in let in statements
      ``` haskell
      fn = let  <- This line break is the first bool in the list
             x
           in   <- This line break is the second bool in the list
             y
      ```
  * `list-comp-specialization` (`false`): Whether to apply some specialization to padding of list comprehension-likes
      ``` haskell
      fn = <-  This newline is controlled by this config 
        [ r
        | r <- a
        ]
      ```
  * `compact-vertical-list` (`Nothing`): Whether to make compact single (`[true, false]`) and 1 element (`[false, true]`) lists
  * `simple-type-app` (`False`): Whether to disable TyApp flattening and splitting. Need to fix a dramatic slowdown in large constraint lists before removing this config
  * `alt-padding` (`False`): Whether to add a newline after -> in alt when the RHS is multiline measured

#### Sorting Imports

There are three import sorting modes:

  * No sorting (`sort-imports: false`).  This mode will retain the
    order of imports and any blank lines between imports.

  * Sort imports and group by the first component of the module path
    (`sort-imports: true`): Data.Maybe goes with Data.Either,
    Control.Monad goes with Control.Applicative.

    This mode is equivalent to:

    ``` json
    "sort-imports": [
      {
        "prefixes": [""],
        "order": "grouped"
      }
    ]
    ```

  * Configurable grouping and sorting (`sort-imports:
    [...]`). `sort-imports` is a list of group declarations, where
    each group defines a set of module prefixes and an order.

    Each import is assigned to a group based on the longest matching
    prefix. Imports without a match are assigned to an implicit final
    group with order `keep`. You can manually collect all left-over
    imports by specifying a group with an empty prefix.

    The order can be one of `keep`, `sorted`, or `grouped`.  `keep`
    retains the manual order of imports of the input file, `sorted`
    will sort imports within the group, and `grouped` will sort and
    then group imports within the group based on the first component
    after the common prefix of the group's prefixes.

    For example:

    ``` json
    "sort-imports": [
      {
        "prefixes": [ "" ],
        "order": "grouped"
      },
      {
        "prefixes": [ "Floskell" ],
        "order": "sorted"
      },
      {
        "prefixes": [ "Test" ],
        "order": "grouped"
      },
      {
        "prefixes": [ "Prelude" ],
        "order": "sorted"
      }
    ]
    ```

    This configuration will first place all 'generic' imports, sorted
    and grouped on the top-level module name component. E.g. all of
    "Control", then "Data", etc.

    After that, there is a sorted section of 'local' imports
    (everything within "Floskell"), without any subdivision.

    Following are all imports from "Test", again sorted and grouped
    based on the first component after "Test".

    Lastly, any imports of "Prelude".
