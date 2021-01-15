# BAP toy BIL

## Toy assembly

There's a dummy file at [resources/test.toy](resources/test.toy):

```
(0x1 block
  (0x2     mov     0x5     r1              )
  (0x3     mov     r1      r2              )
  (0x4     jmp     0x0     0x5             ))

(0x5 block
  (0x6     mov     0x18    r3              )
  (0x7     add     r3      0x3      r4     )
  (0x8     jmp     r4      r1              )
  (0x9     jmp     0x0     0x1             ))
```

Every block starts with a label/address (in this case, `0x1` for the first block, and `0x5` for the second block), and the word `block`.

Then there follows a list of instructions. Each instruction starts with a label/address (e.g., `0x2`, `0x3`, and so on), an operator, and operands. 

Valid operators:
* `mov src dst`: copy the value from `src` into the register `dst`. `src` can be a literal integer (expressed as a hex), or a register.
* `add src1 src2 dst`: add the values from `src1` and `src2`, and copy the result into the register `dst`. Each of `src1` and `src2` can be a literal integer (expressed as a hex), or a register.
* `jmp cond src`: if `cond` is greater than 0, jump to the value in `src`. `cond` can be a literal integer (expressed as a hex), or a register. Likewise, `src` can be a literal integer (hex), or a register.

This is a toy assembly language, so I have only implemented a move instruction, a binary operation, and a conditional jump. Other operators and memory can be added, following the same pattern.


## The parser

This tool will read such toy assembly files, parse them, and convert them into BIL.


## Try it

Clean:

    make clean

Build:

    make build

Look at the toy assembly program at `test.toy`:

    cat resources/test.toy 

Parse it:

    make parse file=resources/test.toy

You will then see both the toy assembly and the BIL printed out.
