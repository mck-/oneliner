One-liner
==============

Extracts a one-liner from a piece of text.

### How?

1. Counts word occurences
2. Given each word, count word occurences immediately after this word
3. Construct sentence by taking 1 and 2 recursively
4. Avoids repeating a word to avoid infinite loops
5. Removes all punctuation marks and convert to lower-case

### Why?

Why not. Are you not curious about the results?

### Example

```
(ql:quickload 'cl-oneliner)
(cl-oneliner:oneliner "whatever wall of text you are too lazy too read")
```

In the functional fashion of recursion, when given this README as an example, it produces the following:

```
"word occurences given each"
```

Isn't that just poetic?

*Idea sparked by @flockonus*
