CLASS zcx_wd_csv_invalid_delimiter DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA:
      delimiter TYPE zcl_wd_csv=>mty_delimiter.
    METHODS:
      constructor IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                            previous  LIKE previous OPTIONAL
                            delimiter TYPE zcl_wd_csv=>mty_delimiter.
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
    IF textid IS INITIAL.
      if_t100_message~t100key-msgid = 'ZWD_CSV'.
      if_t100_message~t100key-msgno = '006'.
      if_t100_message~t100key-attr1 = delimiter.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
