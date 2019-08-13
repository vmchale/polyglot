Plan for the parser:

  1. Split (safely) into arrays using `memchr`.

  2. Skip whitespace characters and look for a line comment or block comment.

  3. If we find a block comment, enter second parse state - a state for block
     comments.
  
  4. Continue looking for block comments, block comment ends, or newlines with
     `strpbrk`.
