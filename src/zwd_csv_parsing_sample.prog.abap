*&---------------------------------------------------------------------*
*& Report zwd_csv_parsing_sample
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwd_csv_parsing_sample.

*=======================================================================
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS:
      shlp_path,
      run,
      pbo,
      pai.
  PRIVATE SECTION.
    DATA:
      mv_delimiter TYPE zcl_wd_csv=>mty_delimiter,
      mv_separator TYPE zcl_wd_csv=>mty_separator,
      mv_endofline TYPE string.
ENDCLASS.

*=======================================================================
DATA:
  go TYPE REF TO lcl.

*=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE bl0_tit.
PARAMETERS path TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK bl0.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE bl1_tit.
PARAMETERS:
  del_dq TYPE flag RADIOBUTTON GROUP rg0 DEFAULT 'X',
  del_sq TYPE flag RADIOBUTTON GROUP rg0.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE bl2_tit.
PARAMETERS septab TYPE flag RADIOBUTTON GROUP rg1 DEFAULT 'X' USER-COMMAND separator.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS sepother TYPE flag RADIOBUTTON GROUP rg1.
SELECTION-SCREEN COMMENT 05(27) FOR FIELD sepother.
PARAMETERS sep TYPE char1 MODIF ID sep.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE bl3_tit.
PARAMETERS cols TYPE i.
SELECTION-SCREEN END OF BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE bl4_tit.
PARAMETERS header TYPE xfeld DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK bl4.

SELECTION-SCREEN BEGIN OF BLOCK bl5 WITH FRAME TITLE bl5_tit.
PARAMETERS:
  crlf TYPE flag RADIOBUTTON GROUP rg2 DEFAULT 'X',
  lf   TYPE flag RADIOBUTTON GROUP rg2,
  cr   TYPE flag RADIOBUTTON GROUP rg2.
SELECTION-SCREEN END OF BLOCK bl5.

*=======================================================================
INITIALIZATION.
  bl0_tit = 'File'(002).
  bl1_tit = 'Column Delimiter'(005).
  bl2_tit = 'Column Separator'(004).
  bl3_tit = 'Columns'(003).
  bl4_tit = 'Header'(006).
  bl5_tit = 'End of Line'(001).
  go = NEW #( ).

*=======================================================================
AT SELECTION-SCREEN ON VALUE-REQUEST FOR path.
  go->shlp_path( ).

*=======================================================================
AT SELECTION-SCREEN OUTPUT.
  go->pbo( ).

*=======================================================================
AT SELECTION-SCREEN.
  go->pai( ).

*=======================================================================
START-OF-SELECTION.
  go->run( ).

*=======================================================================
CLASS lcl IMPLEMENTATION.
  METHOD pbo.
* ---------------------------------------------------------------------
    LOOP AT SCREEN.
      IF screen-group1 = 'SEP'.
        CASE abap_true.
          WHEN septab.
            screen-input = 0.
          WHEN sepother.
            screen-input = 1.
        ENDCASE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD pai.
* ---------------------------------------------------------------------
    CASE abap_true.
      WHEN del_dq.
        mv_delimiter = '"'.
      WHEN del_sq.
        mv_delimiter = ''''.
    ENDCASE.

* ---------------------------------------------------------------------
    CASE abap_true.
      WHEN septab.
        mv_separator = zcl_wd_csv=>mc_default_separator.
      WHEN sepother.
        mv_separator = sep.
    ENDCASE.

* ---------------------------------------------------------------------
    CASE abap_true.
      WHEN crlf.
        mv_endofline = zcl_wd_csv=>mc_endofline_cr_lf.
      WHEN lf.
        mv_endofline = zcl_wd_csv=>mc_endofline_lf.
      WHEN cr.
        mv_endofline = zcl_wd_csv=>mc_endofline_cr.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.

  METHOD shlp_path.
* ---------------------------------------------------------------------
    DATA:
      lt_files  TYPE filetable,
      lv_rc     TYPE i,
      lv_action TYPE i.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>file_open_dialog( EXPORTING  multiselection = abap_false
                                                CHANGING   file_table     = lt_files
                                                           rc             = lv_rc
                                                           user_action    = lv_action
                                                EXCEPTIONS OTHERS         = 1           ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* ---------------------------------------------------------------------
    IF lv_action = cl_gui_frontend_services=>action_ok.
      path = lt_files[ 1 ]-filename.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD run.
* ---------------------------------------------------------------------
    DATA:
      lo_csv_file   TYPE REF TO zcl_wd_csv_file,
      lt_components TYPE cl_abap_structdescr=>component_table,
      lr_data       TYPE REF TO data,
      lx            TYPE REF TO cx_root.
    FIELD-SYMBOLS:
      <lt_data> TYPE STANDARD TABLE.

* ---------------------------------------------------------------------
    DO cols TIMES.
      APPEND VALUE #( name = 'COL_' && sy-index
                      type = cl_abap_elemdescr=>get_string( )
                    ) TO lt_components.
    ENDDO.

* ---------------------------------------------------------------------
    TRY.
        DATA(lo_tabledescr) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_components ) ).
      CATCH cx_root INTO lx.
        MESSAGE lx TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    CREATE DATA lr_data TYPE HANDLE lo_tabledescr.
    ASSIGN lr_data->* TO <lt_data>.

* ---------------------------------------------------------------------
    TRY.
        lo_csv_file = NEW #( iv_endofline = mv_endofline
                             iv_separator = mv_separator
                             iv_delimiter = mv_delimiter ).
        lo_csv_file->parse_file_local( EXPORTING iv_has_header = header
                                                 iv_path       = path
                                       IMPORTING et_data       = <lt_data> ).
      CATCH cx_root INTO lx.
        MESSAGE lx TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING  t_table      = <lt_data>           ).

        LOOP AT lo_salv_table->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<ls_col>).
          <ls_col>-r_column->set_short_text( CONV #( sy-tabix ) ).
        ENDLOOP.

        lo_salv_table->display( ).
      CATCH cx_salv_msg INTO lx.
        MESSAGE lx TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

* ---------------------------------------------------------------------
  ENDMETHOD.



ENDCLASS.
