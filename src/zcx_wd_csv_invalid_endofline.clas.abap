CLASS zcx_wd_csv_invalid_endofline DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA:
      end_of_line TYPE string.
    METHODS:
      constructor IMPORTING textid      LIKE if_t100_message=>t100key OPTIONAL
                            previous    LIKE previous OPTIONAL
                            end_of_line TYPE string.
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
    IF textid IS INITIAL.
      if_t100_message~t100key-msgid = 'ZWD_CSV'.
      if_t100_message~t100key-msgno = '001'.
      if_t100_message~t100key-attr1 = end_of_line.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
