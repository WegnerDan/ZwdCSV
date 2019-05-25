CLASS zcx_wd_csv_invalid_separator DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_invalid_separator,
        msgid TYPE symsgid VALUE 'ZWD_CSV',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'SEPARATOR',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_wd_csv_invalid_separator.
    DATA:
      separator TYPE zcl_wd_csv=>mty_separator READ-ONLY.
    METHODS:
      constructor IMPORTING separator TYPE zcl_wd_csv=>mty_separator.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_invalid_separator IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    me->separator = separator.

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    if_t100_message~t100key = zcx_wd_csv_invalid_separator.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
