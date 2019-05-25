CLASS zcx_wd_csv_gui_download_failed DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_gui_download_failed,
        msgid TYPE symsgid VALUE 'SY-MSGID',
        msgno TYPE symsgno VALUE '999',
        attr1 TYPE scx_attrname VALUE 'SYST-MSGV1',
        attr2 TYPE scx_attrname VALUE 'SYST-MSGV2',
        attr3 TYPE scx_attrname VALUE 'SYST-MSGV3',
        attr4 TYPE scx_attrname VALUE 'SYST-MSGV4',
      END OF zcx_wd_csv_gui_download_failed.
    DATA:
      syst TYPE sy READ-ONLY.
    METHODS:
      constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_wd_csv_gui_download_failed IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
* ---------------------------------------------------------------------
    super->constructor( previous = previous ).

* ---------------------------------------------------------------------
    CLEAR me->textid.

* ---------------------------------------------------------------------
    IF sy-msgid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      me->syst = sy.
      if_t100_message~t100key       = zcx_wd_csv_gui_download_failed.
      if_t100_message~t100key-msgid = sy-msgid.
      if_t100_message~t100key-msgno = sy-msgno.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
