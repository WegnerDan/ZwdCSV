CLASS zcx_wd_csv_invalid_endofline DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_invalid_endofline,
        msgid TYPE symsgid VALUE 'ZWD_CSV',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'END_OF_LINE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_wd_csv_invalid_endofline.
    DATA:
      end_of_line TYPE string READ-ONLY.
    METHODS:
      constructor IMPORTING end_of_line TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_invalid_endofline IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    me->end_of_line = end_of_line.

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    if_t100_message~t100key = zcx_wd_csv_invalid_endofline.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
