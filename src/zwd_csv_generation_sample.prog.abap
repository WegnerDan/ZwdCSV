*&---------------------------------------------------------------------*
*& Report zwd_csv_generation_sample
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwd_csv_generation_sample.

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
PARAMETERS:
  table TYPE tabname,
  rows  TYPE i DEFAULT 500.
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
  bl0_tit = 'File'(001).
  bl1_tit = 'Column Delimiter'(002).
  bl2_tit = 'Column Separator'(003).
  bl3_tit = 'Table'(004).
  bl4_tit = 'Header'(005).
  bl5_tit = 'End of Line'(006).
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
      files    TYPE filetable,
      rc       TYPE i,
      action   TYPE i,
      filename TYPE string,
      path     TYPE string,
      fullpath TYPE string.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>file_save_dialog( CHANGING   filename    = filename
                                                           path        = path
                                                           fullpath    = fullpath
                                                           user_action = action
                                                EXCEPTIONS OTHERS      = 1           ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* ---------------------------------------------------------------------
    IF action = cl_gui_frontend_services=>action_ok.
      path = fullpath.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD run.
* ---------------------------------------------------------------------
    DATA:
      typedescr    TYPE REF TO cl_abap_typedescr,
      structdescr  TYPE REF TO cl_abap_structdescr,
      tabledescr   TYPE REF TO cl_abap_tabledescr,
      csv_file     TYPE REF TO zcl_wd_csv_file,
      components   TYPE cl_abap_structdescr=>component_table,
      source_table TYPE REF TO data,
      error        TYPE REF TO cx_root.
    FIELD-SYMBOLS:
      <source_table> TYPE STANDARD TABLE.

* ---------------------------------------------------------------------
    " fake select to check table name
    TRY.
        SELECT COUNT(*) UP TO 1 ROWS FROM (table).
      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    typedescr = cl_abap_structdescr=>describe_by_name( table ).
    TRY.
        tabledescr = cl_abap_tabledescr=>create( CAST #( typedescr ) ).
      CATCH cx_sy_table_creation INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    CREATE DATA source_table TYPE HANDLE tabledescr.
    ASSIGN source_table->* TO <source_table>.

* ---------------------------------------------------------------------
    TRY.
        SELECT * UP TO @rows ROWS
        FROM (table)
        INTO TABLE @<source_table>
        ORDER BY PRIMARY KEY.
      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    TRY.
        csv_file = NEW #( endofline = endofline
                          separator = separator
                          delimiter = delimiter ).
        csv_file->generate_file_local( with_header  = header
                                       source_table = <source_table>
                                       path         = path      ).
      CATCH cx_root INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
