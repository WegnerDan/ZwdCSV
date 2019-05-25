CLASS zcl_wd_csv_file DEFINITION PUBLIC INHERITING FROM zcl_wd_csv CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_encoding    TYPE abap_encod    DEFAULT '4110'
                            iv_replacement TYPE abap_repl     DEFAULT '#'
                            iv_ignore_cerr TYPE abap_bool     DEFAULT abap_true
                            iv_endofline   TYPE csequence     DEFAULT mc_endofline_cr_lf
                            iv_separator   TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter   TYPE mty_delimiter DEFAULT mc_default_delimiter
                            iv_conv_exit   TYPE abap_bool     DEFAULT abap_false
                  RAISING   zcx_wd_csv_invalid_endofline
                            zcx_wd_csv_invalid_separator
                            zcx_wd_csv_invalid_delimiter,
      parse_file_appl IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                                iv_path       TYPE string
                      EXPORTING et_data       TYPE STANDARD TABLE
                      RAISING   cx_sy_struct_creation
                                cx_sy_conversion_error
                                cx_sy_file_open
                                cx_sy_codepage_converter_init
                                cx_sy_file_authority
                                cx_sy_file_io
                                cx_sy_file_open_mode
                                cx_sy_file_close
                                cx_parameter_invalid_range
                                cx_parameter_invalid_type
                                RESUMABLE(zcx_wd_csv_too_many_columns)
                                RESUMABLE(zcx_wd_csv_too_few_columns)
                                RESUMABLE(zcx_wd_csv_mixed_endofline),
      parse_file_local IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                                 iv_path       TYPE string
                       EXPORTING et_data       TYPE STANDARD TABLE
                       RAISING   zcx_wd_csv_gui_upload_failed
                                 cx_sy_conversion_error
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_parameter_invalid_type
                                 cx_sy_struct_creation
                                 RESUMABLE(zcx_wd_csv_too_many_columns)
                                 RESUMABLE(zcx_wd_csv_too_few_columns)
                                 RESUMABLE(zcx_wd_csv_mixed_endofline),
      generate_file_appl IMPORTING iv_with_header TYPE abap_bool DEFAULT abap_false
                                   it_data        TYPE STANDARD TABLE
                                   iv_path        TYPE string
                         RAISING   cx_parameter_invalid_range
                                   cx_sy_codepage_converter_init
                                   cx_sy_conversion_codepage
                                   cx_parameter_invalid_type
                                   cx_sy_file_open
                                   cx_sy_file_authority
                                   cx_sy_file_io
                                   cx_sy_file_open_mode
                                   cx_sy_file_close,
      generate_file_local IMPORTING iv_with_header TYPE abap_bool DEFAULT abap_false
                                    it_data        TYPE STANDARD TABLE
                                    iv_path        TYPE string
                          RAISING   zcx_wd_csv_gui_download_failed
                                    cx_parameter_invalid_range
                                    cx_sy_codepage_converter_init
                                    cx_sy_conversion_codepage
                                    cx_parameter_invalid_type.
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
                                cx_parameter_invalid_type,
      encode_string IMPORTING iv_string  TYPE string
                    EXPORTING ev_xstring TYPE xstring
                    RAISING   cx_parameter_invalid_range
                              cx_sy_codepage_converter_init
                              cx_sy_conversion_codepage
                              cx_parameter_invalid_type,
      write_file_appl IMPORTING iv_path       TYPE string
                                iv_csv_string TYPE string
                      RAISING   cx_parameter_invalid_range
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_parameter_invalid_type
                                cx_sy_file_open
                                cx_sy_file_authority
                                cx_sy_file_io
                                cx_sy_file_open_mode
                                cx_sy_file_close,
      write_file_local IMPORTING iv_path       TYPE string
                                 iv_csv_string TYPE string
                       RAISING   zcx_wd_csv_gui_download_failed
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_sy_conversion_codepage
                                 cx_parameter_invalid_type.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv_file IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    super->constructor( iv_endofline = iv_endofline
                        iv_separator = iv_separator
                        iv_delimiter = iv_delimiter
                        iv_conv_exit = iv_conv_exit ).

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


  METHOD encode_string.
* ---------------------------------------------------------------------
    cl_abap_conv_out_ce=>create( encoding    = mv_encoding
                                 replacement = mv_replacement
                                 ignore_cerr = mv_ignore_cerr )->convert( EXPORTING data   = iv_string
                                                                          IMPORTING buffer = ev_xstring ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_file_appl.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_string TYPE string.

* ---------------------------------------------------------------------
    generate_string( EXPORTING iv_with_header = iv_with_header
                               it_data        = it_data
                     IMPORTING ev_csv_string  = lv_csv_string  ).

* ---------------------------------------------------------------------
    write_file_appl( iv_path       = iv_path
                     iv_csv_string = lv_csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_string TYPE string.

* ---------------------------------------------------------------------
    generate_string( EXPORTING iv_with_header = iv_with_header
                               it_data        = it_data
                     IMPORTING ev_csv_string  = lv_csv_string  ).

* ---------------------------------------------------------------------
    write_file_local( iv_path       = iv_path
                      iv_csv_string = lv_csv_string ).

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
      lv_csv_xstring TYPE xstring.

* ---------------------------------------------------------------------
    OPEN DATASET iv_path FOR INPUT IN BINARY MODE.

* ---------------------------------------------------------------------
    READ DATASET iv_path INTO lv_csv_xstring.

* ---------------------------------------------------------------------
    CLOSE DATASET iv_path.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING iv_xstring = lv_csv_xstring
                    IMPORTING ev_string  = ev_csv_string  ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_filelength  TYPE i,
      lt_data        TYPE solix_tab,
      lv_csv_xstring TYPE xstring.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = iv_path
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = lv_filelength
                                          CHANGING   data_tab   = lt_data
                                          EXCEPTIONS OTHERS     = 1             ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_csv_gui_upload_failed.
    ENDIF.

* ---------------------------------------------------------------------
    lv_csv_xstring = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data
                                                       iv_size  = lv_filelength ).

* ---------------------------------------------------------------------
    " free up some memory
    FREE lt_data.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING iv_xstring = lv_csv_xstring
                    IMPORTING ev_string  = ev_csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD write_file_appl.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_xstring TYPE xstring.

* ---------------------------------------------------------------------
    encode_string( EXPORTING iv_string  = iv_csv_string
                   IMPORTING ev_xstring = lv_csv_xstring ).

* ---------------------------------------------------------------------
    OPEN DATASET iv_path FOR OUTPUT IN BINARY MODE.

* ---------------------------------------------------------------------
    TRANSFER lv_csv_xstring TO iv_path.

* ---------------------------------------------------------------------
    CLOSE DATASET iv_path.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD write_file_local.
* ---------------------------------------------------------------------
    DATA:
      lv_csv_xstring TYPE xstring,
      lt_data        TYPE solix_tab,
      lv_filelength  TYPE i.

* ---------------------------------------------------------------------
    encode_string( EXPORTING iv_string  = iv_csv_string
                   IMPORTING ev_xstring = lv_csv_xstring ).

* ---------------------------------------------------------------------
    lv_filelength = xstrlen( lv_csv_xstring ).
    lt_data       = cl_bcs_convert=>xstring_to_solix( lv_csv_xstring ).

* ---------------------------------------------------------------------
    FREE lv_csv_xstring.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_filelength
                                                       filename     = iv_path
                                                       filetype     = 'BIN'
                                            CHANGING   data_tab     = lt_data
                                            EXCEPTIONS OTHERS       = 1              ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_csv_gui_download_failed.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
