{-# LANGUAGE QuasiQuotes #-}
module Command.Help where

import Types
import Opts.Opts
import Control.Monad.State (liftIO)
import Text.Heredoc

showHelp :: HelpObject -> ST ()
showHelp HAccessMode = (liftIO . putStr) accessModeMsg
showHelp HTemplate = (liftIO . putStr) templateMsg
showHelp HEnv = (liftIO . putStr) environmentMsg

accessModeMsg :: String
accessModeMsg = [here|
  File with extension .fix_mode store access mode for all files
  Format:
  
  # comment here
  # umask (man umask)
  umask:022
  
  # for all directory in /etc
  # filepath with regexp:owner:group:access mode for files:access mode for directories
  /etc/.*:user:group::755
  # for all files in /etc
  /etc/.*:user:group:644:
  
  # for all files in subpath 
  /etc/nginx/*:root:root:644:755
  |]

templateMsg :: String
templateMsg = [here|
    File with extension .ede is template
    file.conf.ede => file.conf

        Expressions
        Expressions behave as any simplistic programming language with a variety of prefix, infix, and postifx operators available. (See: Text.EDE.Filters)
        
        A rough overview of the expression grammar:
        
        expression ::= literal | identifier | '|' filter
        filter     ::= identifier
        identifier ::= [a-zA-Z_]{1}[0-9A-Za-z_']*
        object     ::= '{' pairs '}'
        pairs      ::= string ':' literal | string ':' literal ',' pairs
        array      ::= '[' elements ']'
        elements   ::= literal | literal ',' elements
        literal    ::= object | array | boolean | number | string
        boolean    ::= true | false
        number     ::= integer | double
        string     ::= "char+|escape"
        Variables
        Variables are substituted directly for their renderable representation. An error is raised if the varaible being substituted is not a literal type (ie. an Array or Object) or doesn't exist in the supplied environment.
        
        {{ var }}
        Nested variable access is also supported for variables which resolve to an Object. Dot delimiters are used to chain access through multiple nested Objects. The right-most accessor must resolve to a renderable type as with the previous non-nested variable access.
        
        {{ nested.var.access }}
        Conditionals
        A conditional is introduced and completed with the section syntax:
        
        {% if <expr1> %}
           ... consequent expressions
        {% elif <expr2> %}
           ... consequent expressions
        {% elif <expr3> %}
           ... consequent expressions
        {% else %}
           ... alternate expressions
        {% endif %}
        The boolean result of the expr determines the branch that is rendered by the template with multiple (or none) elif branches supported, and the else branch being optional.
        
        In the case of a literal it conforms directly to the supported boolean or relation logical operators from Haskell. If a variable is singuarly used its existence determines the result of the predicate, the exception to this rule is boolean values which will be substituted into the expression if they exist in the supplied environment.
        
        The following logical expressions are supported as predicates in conditional statements with parameters type checked and an error raised if the left and right hand sides are not type equivalent.
        
        And: &&
        Or: ||
        Equal: ==
        Not Equal: != (See: /=)
        Greater: >
        Greater Or Equal: >=
        Less: <
        Less Or Equal: <=
        Negation: ! (See: not)
        See: Text.EDE.Filters
        
        Case Analysis
        To pattern match a literal or variable, you can use the case statement:
        
        {% case var %}
        {% when "a" %}
           .. matched expressions
        {% when "b" %}
           .. matched expressions
        {% else %}
           .. alternate expressions
        {% endcase %}
        Patterns take the form of variables, literals, or the wild-card '@_@' pattern (which matches anything).
        
        Loops
        Iterating over an Array or Object can be acheived using the 'for ... in' section syntax. Attempting to iterate over any other type will raise an error.
        
        Example:
        
        {% for var in list %}
            ... iteration expression
        {% else %}
            ... alternate expression
        {% endfor %}
        The iteration branch is rendering per item with the else branch being (which is optional) being rendered if the {{ list }} variable is empty.
        
        When iterating over an Object, a stable sort using key equivalence is applied, Arrays are unmodified.
        
        The resulting binding within the iteration expression (in this case, {{ var }}) is an Object containing the following keys:
        
        key :: Text: They key if the loop target is an Object
        value :: a: The value of the loop target
        loop :: Object: Loop metadata.
        length :: Int: Length of the loop
        index :: Int: Index of the iteration
        index0 :: Int: Zero based index of the iteration
        remainder :: Int: Remaining number of iterations
        remainder0 :: Int: Zero based remaining number of iterations
        first :: Bool: Is this the first iteration?
        last :: Bool: Is this the last iteration?
        odd :: Bool: Is this an odd iteration?
        even :: Bool: Is this an even iteration?
        For example:
        
        {% for item in items %}
            {{ item.index }}:{{ item.value }}
            {% if !item.last %}
        
            {% endif %}
        {% endfor %}
        Will render each item with its (1-based) loop index as a prefix, separated by a blank newline, without a trailing at the end of the document.
        
        Valid loop targets are Objects, Arrays, and Strings, with only Objects having an available {{ var.key }} in scope.
        Raw
        You can disable template processing for blocks of text using the raw section:
        
        {% raw %}
        Some {{{ handlebars }}} or {{ mustache }} or {{ jinja2 }} output tags etc.
        {% endraw %}
        This can be used to avoid parsing expressions which would otherwise be considered valid ED-E syntax.
        
        Comments
        Comments are ignored by the parser and omitted from the rendered output.
        
        {# singleline comment #}
        {#
           multiline
           comment
        #}
        Let Expressions
        You can also bind an identifier to values which will be available within the following expression scope.
        
        For example:
        
        {% let var = false %}
        ...
        {{ var }}
        ...
    |]

environmentMsg :: String
environmentMsg = [here|
    File with extension .fix_env is environment
    Environment containing Objects for template.
    Format: just yaml
    |]

