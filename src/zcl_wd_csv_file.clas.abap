CLASS zcl_wd_csv_file DEFINITION PUBLIC INHERITING FROM zcl_wd_csv CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_encoding    TYPE abap_encod    DEFAULT '4110'
                            iv_replacement TYPE abap_repl     DEFAULT '#'
                            iv_ignore_cerr TYPE abap_bool     DEFAULT abap_true
                            iv_newline     TYPE mty_newline   DEFAULT mc_default_newline
                            iv_separator   TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter   TYPE mty_delimiter DEFAULT mc_default_delimiter
                  RAISING   zcx_wd_csv_invalid_newline,
      parse_file_appl IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                                iv_path       TYPE string
                      EXPORTING et_data       TYPE table
                      RAISING   cx_sy_struct_creation
                                cx_sy_file_open
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_sy_file_authority
                                cx_sy_file_io
                                cx_sy_file_open_mode
                                cx_sy_file_close
                                cx_parameter_invalid_range
                                cx_parameter_invalid_type,
      parse_file_local IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                                 iv_path       TYPE string
                       EXPORTING et_data       TYPE table
                       RAISING   zcx_wd_csv_gui_upload_failed
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_sy_conversion_codepage
                                 cx_parameter_invalid_type
                                 cx_sy_struct_creation.
  PROTECTED SECTION.
    DATA:
      mv_encoding    TYPE abap_encod,
      mv_replacement TYPE abap_repl,
      mv_ignore_cerr TYPE abap_bool.
    METHODS:
      decode_xstring IMPORTING iv_xstring TYPE xstring
                     EXPORTING ev_string  TYPE string
                     RAISING   cx_parameter_invalid_range
                               cx_sy_codepage_converter_init
                               cx_sy_conversion_codepage
                               cx_parameter_invalid_type,
      read_file_appl IMPORTING iv_path       TYPE string
                     EXPORTING ev_csv_string TYPE string
                     RAISING   cx_sy_file_open
                               cx_sy_codepage_converter_init
                               cx_sy_conversion_codepage
                               cx_sy_file_authority
                               cx_sy_file_io
                               cx_sy_file_open_mode
                               cx_sy_file_close,
      read_file_local IMPORTING iv_path       TYPE string
                      EXPORTING ev_csv_string TYPE string
                      RAISING   zcx_wd_csv_gui_upload_failed
                                cx_parameter_invalid_range
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_parameter_invalid_type.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WD_CSV_FILE IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    super->constructor( iv_newline   = iv_newline
                        iv_separator = iv_separator
                        iv_delimiter = iv_delimiter ).

* ---------------------------------------------------------------------
    mv_encoding    = iv_encoding.
    mv_replacement = iv_replacement.
    mv_ignore_cerr = iv_ignore_cerr.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD decode_xstring.
* ---------------------------------------------------------------------
    cl_abap_conv_in_ce=>create( encoding    = mv_encoding
                                replacement = mv_replacement
                                ignore_cerr = mv_ignore_cerr )->convert( EXPORTING input = iv_xstring
                                                                         IMPORTING data  = ev_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_appl.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_string TYPE string.

* ---------------------------------------------------------------------
    read_file_appl( EXPORTING iv_path       = iv_path
                    IMPORTING ev_csv_string = lv_csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING iv_has_header = iv_has_header
                            iv_csv_string = lv_csv_string
                  IMPORTING et_data       = et_data       ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_string TYPE string.

* ---------------------------------------------------------------------
    read_file_local( EXPORTING iv_path       = iv_path
                     IMPORTING ev_csv_string = lv_csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING iv_has_header = iv_has_header
                            iv_csv_string = lv_csv_string
                  IMPORTING et_data       = et_data       ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_appl.
* ---------------------------------------------------------------------
    DATA:
      lv_xstring TYPE xstring.

* ---------------------------------------------------------------------
    OPEN DATASET iv_path FOR INPUT IN BINARY MODE.

* ---------------------------------------------------------------------
    READ DATASET iv_path INTO lv_xstring.

* ---------------------------------------------------------------------
    CLOSE DATASET iv_path.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING iv_xstring = lv_xstring
                    IMPORTING ev_string  = ev_csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_filelength TYPE i,
      lt_data       TYPE solix_tab,
      lv_xstring    TYPE xstring,
      lo_conv       TYPE REF TO cl_abap_conv_in_ce.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = iv_path
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = lv_filelength
                                          CHANGING   data_tab   = lt_data
                                          EXCEPTIONS OTHERS     = 1             ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_csv_gui_upload_failed
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = sy-msgv1
                            attr2 = sy-msgv2
                            attr3 = sy-msgv3
                            attr4 = sy-msgv4 ).
    ENDIF.

* ---------------------------------------------------------------------
    lv_xstring = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data
                                                   iv_size  = lv_filelength ).

* ---------------------------------------------------------------------
    " free up some memory
    FREE lt_data.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING iv_xstring = lv_xstring
                    IMPORTING ev_string  = ev_csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
