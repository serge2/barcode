barcode
=====
The library for barcode images generation

Supported types:
- code128  (ascii symbols, including control chars)
- ucp_a    (11 digits + 1 check)
- ean_13   (12 digits + 1 check)
- ean_8    (7 digits + 1 check)
- std_2_of_5 (variable number of digits + 1 check)


How to use
-----

    $ rebar3 shell
    > barcode:save_png(code128, "Hello, world!", "code128.png").
    ok

    > barcode:save_text(code128, "Hello, world!", "code128.txt").
    ok

    > barcode:png_encode(code128, "Hello, world!").
    <<...>>

    > barcode:text_encode(code128, "Hello, world!").
    <<...>>
