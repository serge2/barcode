barcode
=====
The library for barcode images generation

Supported types:
- code128b (most ascii symbols)
- ucp_a    (11 digits + 1 check)
- ean_13   (12 digits + 1 check)
- ean_8    (7 digits + 1 check)
- std_2_of_5 (variable number of digits + 1 check)


How to use
-----

    $ rebar3 shell
    > barcode:save_png(code128b, "Hello, world!", "code128b.png").
    ok

    > barcode:save_text(code128b, "Hello, world!", "code128b.txt").
    ok

    > barcode:png_encode(code128b, "Hello, world!").
    <<...>>

    > barcode:text_encode(code128b, "Hello, world!").
    <<...>>
