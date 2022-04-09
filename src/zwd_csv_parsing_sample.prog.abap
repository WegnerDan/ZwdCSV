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
      delimiter TYPE zcl_wd_csv=>ty_delimiter,
      separator TYPE zcl_wd_csv=>ty_separator,
      endofline TYPE string.
ENDCLASS.

*=======================================================================
DATA:
  go TYPE REF TO lcl.

*=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE bl0_tit.
PARAMETERS path TYPE string LOWER CASE OBLIGATORY.
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
        delimiter = zcl_wd_csv=>c_delimiter_double_quote.
      WHEN del_sq.
        delimiter = zcl_wd_csv=>c_delimiter_single_quote.
    ENDCASE.

* ---------------------------------------------------------------------
    CASE abap_true.
      WHEN septab.
        separator = zcl_wd_csv=>c_separator_tab.
      WHEN sepother.
        separator = sep.
    ENDCASE.

* ---------------------------------------------------------------------
    CASE abap_true.
      WHEN crlf.
        endofline = zcl_wd_csv=>c_endofline_cr_lf.
      WHEN lf.
        endofline = zcl_wd_csv=>c_endofline_lf.
      WHEN cr.
        endofline = zcl_wd_csv=>c_endofline_cr.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.

  METHOD shlp_path.
* ---------------------------------------------------------------------
    DATA:
      files  TYPE filetable,
      rc     TYPE i,
      action TYPE i.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>file_open_dialog( EXPORTING  multiselection = abap_false
                                                CHANGING   file_table     = files
                                                           rc             = rc
                                                           user_action    = action
                                                EXCEPTIONS OTHERS         = 1           ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* ---------------------------------------------------------------------
    IF action = cl_gui_frontend_services=>action_ok.
      path = files[ 1 ]-filename.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD run.
* ---------------------------------------------------------------------
    DATA:
      csv_file     TYPE REF TO zcl_wd_csv_file,
      components   TYPE cl_abap_structdescr=>component_table,
      target_table TYPE REF TO data,
      error        TYPE REF TO cx_root,
      answer       TYPE c LENGTH 1.
    FIELD-SYMBOLS:
      <target_table> TYPE STANDARD TABLE.

* ---------------------------------------------------------------------
    DO cols TIMES.
      APPEND VALUE #( name = 'COL_' && sy-index
                      type = cl_abap_elemdescr=>get_string( )
                    ) TO components.
    ENDDO.

* ---------------------------------------------------------------------
    TRY.
        DATA(lo_tabledescr) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( components ) ).
      CATCH cx_root INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    CREATE DATA target_table TYPE HANDLE lo_tabledescr.
    ASSIGN target_table->* TO <target_table>.

* ---------------------------------------------------------------------
    TRY.
        csv_file = NEW #( endofline = endofline
                          separator = separator
                          delimiter = delimiter ).
        csv_file->parse_file_local( EXPORTING has_header = header
                                              path       = path
                                    IMPORTING target_table = <target_table> ).
      CATCH BEFORE UNWIND zcx_wd_csv_too_many_columns
                          zcx_wd_csv_too_few_columns
                          zcx_wd_csv_mixed_endofline INTO error.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = error->get_text( ) && ` ` && 'Continue?'(007)
            display_cancel_button = abap_false
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = answer.
        IF answer = 1.
          RESUME.
        ELSE.
          RETURN.
        ENDIF.
      CATCH cx_root INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
                                CHANGING  t_table      = <target_table> ).

        DATA(header_columns) = csv_file->get_header_columns( ).

        LOOP AT salv_table->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<col>).
          CASE header.
            WHEN abap_true.
              TRY.
                  <col>-r_column->set_short_text( CONV #( header_columns[ sy-tabix ]-name ) ).
                CATCH cx_sy_itab_line_not_found.
                  <col>-r_column->set_short_text( CONV #( sy-tabix ) ).
              ENDTRY.
            WHEN abap_false.
              <col>-r_column->set_short_text( CONV #( sy-tabix ) ).
          ENDCASE.
        ENDLOOP.

        salv_table->display( ).
      CATCH cx_salv_msg INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

* ---------------------------------------------------------------------
  ENDMETHOD.



ENDCLASS.
