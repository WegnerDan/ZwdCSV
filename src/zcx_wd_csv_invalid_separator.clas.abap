CLASS zcx_wd_csv_invalid_separator DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA:
      separator TYPE zcl_wd_csv=>mty_separator.
    METHODS:
      constructor IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                            previous  LIKE previous OPTIONAL
                            separator TYPE zcl_wd_csv=>mty_separator.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_WD_CSV_INVALID_SEPARATOR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    me->separator = separator.

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    IF textid IS INITIAL.
      if_t100_message~t100key-msgid = 'ZWD_CSV'.
      if_t100_message~t100key-msgno = '005'.
      if_t100_message~t100key-attr1 = separator.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
