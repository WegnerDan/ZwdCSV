CLASS zcx_wd_csv_gui_download_failed DEFINITION PUBLIC INHERITING FROM zcx_wd_csv_base FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_wd_csv_gui_download_failed,
        msgid TYPE symsgid VALUE '00',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'SYST-MSGV1' ##MG_MISSING,
        attr2 TYPE scx_attrname VALUE 'SYST-MSGV2' ##MG_MISSING,
        attr3 TYPE scx_attrname VALUE 'SYST-MSGV3' ##MG_MISSING,
        attr4 TYPE scx_attrname VALUE 'SYST-MSGV4' ##MG_MISSING,
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
