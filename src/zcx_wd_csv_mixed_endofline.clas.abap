CLASS zcx_wd_csv_mixed_endofline DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_malformed FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_invalid_endofline,
        msgid TYPE symsgid VALUE 'ZWD_CSV',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'LINE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_wd_csv_invalid_endofline.
    METHODS:
      constructor IMPORTING line TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_mixed_endofline IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous
                        line     = line     ).

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    if_t100_message~t100key = zcx_wd_csv_invalid_endofline.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
