CLASS zcx_wd_csv_mixed_endofline DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_malformed FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING textid   LIKE if_t100_message=>t100key OPTIONAL
                            previous LIKE previous OPTIONAL
                            line     TYPE i OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_mixed_endofline IMPLEMENTATION.
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
