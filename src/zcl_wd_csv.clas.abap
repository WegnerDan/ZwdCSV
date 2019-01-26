CLASS zcl_wd_csv DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      mty_separator TYPE c LENGTH 1,
      mty_delimiter TYPE c LENGTH 1,
      mty_newline   TYPE string.
    CONSTANTS:
      mc_default_separator TYPE mty_separator VALUE cl_abap_char_utilities=>horizontal_tab,
      mc_default_delimiter TYPE mty_delimiter VALUE '"',
      mc_default_newline   TYPE mty_newline VALUE cl_abap_char_utilities=>cr_lf.
    METHODS:
      constructor IMPORTING iv_separator TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter TYPE mty_delimiter DEFAULT mc_default_delimiter
                            iv_newline   TYPE mty_newline   DEFAULT mc_default_newline,
      parse_file_appl IMPORTING iv_path TYPE string
                      EXPORTING et_data TYPE table,
      parse_file_local IMPORTING iv_path TYPE string
                       EXPORTING et_data TYPE table,
      parse_string IMPORTING iv_csv_string TYPE string
                   EXPORTING et_data       TYPE table.
  PROTECTED SECTION.
    DATA:
      mv_separator TYPE mty_separator,
      mv_delimiter TYPE mty_delimiter,
      mv_newline   TYPE mty_newline.
    METHODS:
      create_string_struc IMPORTING it_data             TYPE ANY TABLE
                          RETURNING VALUE(rd_str_struc) TYPE REF TO data,
      read_file_appl IMPORTING iv_path TYPE string
                     EXPORTING ev_csv  TYPE string,
      read_file_local IMPORTING iv_path TYPE string
                      EXPORTING ev_csv  TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    mv_separator = iv_separator.
    mv_delimiter = iv_delimiter.
    mv_newline   = iv_newline.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_appl.
* ---------------------------------------------------------------------

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_string TYPE string.

* ---------------------------------------------------------------------
    read_file_local( EXPORTING iv_path = iv_path
                     IMPORTING ev_csv  = lv_csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING iv_csv_string = lv_csv_string
                  IMPORTING et_data       = et_data       ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_string.
* ---------------------------------------------------------------------
    DATA:
      lv_str_length TYPE i,
      lv_str_pos    TYPE i,
      lv_component  TYPE i VALUE 1,
      lv_delimited  TYPE abap_bool,
      ld_str_struc  TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_data_str> TYPE any,  " temporary structure with string types components
      <ls_data_exp> TYPE any,  " line of export table
      <lv_data>     TYPE data. " character

    DEFINE append_line.
      IF <ls_data_exp> IS ASSIGNED. " is unassigned when appending first line
        MOVE-CORRESPONDING <ls_data_str> TO <ls_data_exp>.
        FREE <ls_data_str>.
      ENDIF.
      APPEND INITIAL LINE TO et_data ASSIGNING <ls_data_exp>.
      ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
    END-OF-DEFINITION.

    DEFINE append_character.
      <lv_data> = <lv_data> && iv_csv_string+lv_str_pos(1).
    END-OF-DEFINITION.

    DEFINE continue_loop.
      lv_str_pos = lv_str_pos + 1.
      CONTINUE.
    END-OF-DEFINITION.

* ---------------------------------------------------------------------
    ld_str_struc = create_string_struc( et_data ).
    ASSIGN ld_str_struc->* TO <ls_data_str>.

* ---------------------------------------------------------------------
    lv_str_length = strlen( iv_csv_string ).

* ---------------------------------------------------------------------
    append_line.

* ---------------------------------------------------------------------
    DO.
      CASE iv_csv_string+lv_str_pos(1).
        WHEN mv_delimiter.
          CASE lv_delimited.
            WHEN abap_false.
              lv_delimited = abap_true.
            WHEN abap_true.
              lv_delimited = abap_false.
          ENDCASE.
        WHEN mv_separator.
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          lv_component = lv_component + 1.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
        WHEN mv_newline(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          CASE strlen( mv_newline ).
            WHEN 1.
              lv_component = 1.
              append_line.
            WHEN 2.
              continue_loop.
            WHEN OTHERS.
          ENDCASE.
        WHEN mv_newline+1(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          lv_component = 1.
          append_line.
        WHEN OTHERS.
          append_character.
      ENDCASE.
      IF ( lv_str_pos + 1 ) = lv_str_length.
        EXIT.
      ENDIF.
      lv_str_pos = lv_str_pos + 1.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_appl.

  ENDMETHOD.


  METHOD read_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_filelength TYPE i,
      lt_data       TYPE solix_tab,
      lv_xstring    TYPE xstring,
      lo_conv       TYPE REF TO cl_abap_conv_in_ce.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = iv_path
                                                     filetype                = 'BIN'
                                          IMPORTING  filelength              = lv_filelength
                                          CHANGING   data_tab                = lt_data
                                          EXCEPTIONS file_open_error         = 1
                                                     file_read_error         = 2
                                                     no_batch                = 3
                                                     gui_refuse_filetransfer = 4
                                                     invalid_type            = 5
                                                     no_authority            = 6
                                                     unknown_error           = 7
                                                     bad_data_format         = 8
                                                     header_not_allowed      = 9
                                                     separator_not_allowed   = 10
                                                     header_too_long         = 11
                                                     unknown_dp_error        = 12
                                                     access_denied           = 13
                                                     dp_out_of_memory        = 14
                                                     disk_full               = 15
                                                     dp_timeout              = 16
                                                     not_supported_by_gui    = 17
                                                     error_no_gui            = 18
                                                     OTHERS                  = 19                 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* ---------------------------------------------------------------------
    lv_xstring = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data
                                                   iv_size  = lv_filelength ).

* ---------------------------------------------------------------------
    FREE lt_data.

* ---------------------------------------------------------------------
    lo_conv = cl_abap_conv_in_ce=>create(
    encoding                      = '4110'
*    endian                        =     " Input byte order for numeric data types
*    replacement                   = '#'    " Replacement Character for Character Set Conversion
    ignore_cerr                   = abap_true    " Flag: Ignore Errors When Converting Character Set
*    input                         =     " Input Buffer (X, XSTRING)
    ).
*  CATCH cx_parameter_invalid_range.
*  CATCH cx_sy_codepage_converter_init.
    lo_conv->convert(
      EXPORTING
        input                         = lv_xstring    " Byte sequence to convert
*    n                             = -1    " Number of Units To Be Read
      IMPORTING
        data                          = ev_csv
*    len                           =     " Number of Converted Units
*    input_too_short               =     " Input Buffer Was too Short
    ).
*  CATCH cx_sy_conversion_codepage.    "
*  CATCH cx_sy_codepage_converter_init.    "
*  CATCH cx_parameter_invalid_type.    "

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD create_string_struc.
* ---------------------------------------------------------------------
    " create structure with string components and the same component names
    " as the line type of the export table
* ---------------------------------------------------------------------
    DATA:
      lt_components     TYPE abap_component_view_tab,
      lo_tabledescr     TYPE REF TO cl_abap_tabledescr,
      lo_structdescr    TYPE REF TO cl_abap_structdescr,
      lt_components_str TYPE cl_abap_structdescr=>component_table.

* ---------------------------------------------------------------------
    lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    lt_components = lo_structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
      APPEND VALUE #( name = <ls_component>-name
                      type = cl_abap_elemdescr=>get_string( ) ) TO lt_components_str.
    ENDLOOP.

* ---------------------------------------------------------------------
    lo_structdescr = cl_abap_structdescr=>create( lt_components_str ).
    CREATE DATA rd_str_struc TYPE HANDLE lo_structdescr.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
