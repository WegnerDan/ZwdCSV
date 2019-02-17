CLASS zcx_wd_csv_malformed DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base CREATE PUBLIC.
  PUBLIC SECTION.
    DATA:
      line TYPE i READ-ONLY.
    METHODS:
      constructor IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
                            previous LIKE previous OPTIONAL
                            line     TYPE i OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_malformed IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    me->line = line.

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
