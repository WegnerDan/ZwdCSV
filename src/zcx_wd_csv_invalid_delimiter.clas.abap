CLASS zcx_wd_csv_invalid_delimiter DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_invalid_delimiter,
        msgid TYPE symsgid VALUE 'ZWD_CSV',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'DELIMITER',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_wd_csv_invalid_delimiter.
    DATA:
      delimiter TYPE zcl_wd_csv=>ty_delimiter READ-ONLY.
    METHODS:
      constructor IMPORTING delimiter TYPE zcl_wd_csv=>ty_delimiter.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_invalid_delimiter IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    me->delimiter = delimiter.

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    if_t100_message~t100key = zcx_wd_csv_invalid_delimiter.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
