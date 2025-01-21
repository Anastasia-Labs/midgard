1. The files are organized "vertically," meaning each file is dedicated to a
   concrete purpose/logic rather than language constructs such as types and
   constants (there are a couple of exceptions for files that are likely to be
   modified by multiple developers). This approach should be preserved for any
   new modules/validators.
2. Library modules are generally mapped to the validators on a one-to-one basis.
   Almost all of the library counterparts have dedicated definitions of `Datum`,
   `SpendRedeemer`, `MintRedeemer`, etc. for their corresponding validators.
   This—instead of having datums and redeemers having more explicit names such
   as `SettlementDatum`—allows us to have a consistent interface which makes it
   easier for validators to interact with one another (e.g. one validator
   relying on the execution of a specific endpoint of another validator).
3. Library counterparts are meant to define logically related datatypes and
   helper functions. Therefore aim to always import the libraries and their
   contents directly into their corresponding validators, i.e. don't import the
   dedicated library of a validator in a qualified manner.
4. When it comes to naming, please follow these conventions:
    - Avoid acronyms as much as possible. Use them only for qualified imports or
      "temporary" variables (e.g. inside the body of a small anonymous
      function). In general, try to be as explicit as possible. The goal is to
      keep the code easy to read.
    - Only use `kebab-case` for filenames.
    - For type and data constructors, use `PascalCase`. Avoid using `_` in them.
      If you find yourself needing to have a prefix/postfix, consider breaking
      your file into multiple files.
    - For variables, constants and functions, use `snake_case`.
5. When pattern matching on a record type, always use the curly brackets syntax.
   Using parenthesis won't allow the compiler to detect changes in the accessor
   names of type definitions.
6. Strive to perform `expect`s as soon as possible, rather than including them
   all in a giant `and {}` at the end of your validators/functions.
7. Use enumerated comments right above each `expect`/validation to describe its
   purpose. While comments are prone to going stale, this allows easier
   conversations when discussing particular logics.
