barcode
=====
The library for barcode images generation

Supported types:
- code128       (ascii symbols, including control chars)
- ucp_a         (11 digits + 1 check)
- ean13         (12 digits + 1 check)
- ean8          (7 digits + 1 check)
- code25std     (variable number of digits)
- code25std_crc (variable number of digits + 1 check)
- code25i       (variable number of digits)
- code2icrc     (variable number of digits + 1 check)
- pdf417        (2d, variable number of symbols, ascii, bytes)

How to use
-----

    $ rebar3 shell
    > barcode:save_png(code128, <<"Hello, world!">>, "code128.png").
    ok

    > barcode:png_encode(code128, <<"Hello, world!">>).
    <<...>>

