CLASS zcl_wd_csv_file DEFINITION PUBLIC INHERITING FROM zcl_wd_csv CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING encoding    TYPE abap_encod   DEFAULT '4110'
                            replacement TYPE abap_repl    DEFAULT '#'
                            ignore_cerr TYPE abap_bool    DEFAULT abap_true
                            endofline   TYPE csequence    DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                            separator   TYPE ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                            delimiter   TYPE ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                            conv_exit   TYPE abap_bool    DEFAULT abap_false
                            trim_spaces TYPE abap_bool    DEFAULT abap_false
                  RAISING   zcx_wd_csv_invalid_endofline
                            zcx_wd_csv_invalid_separator
                            zcx_wd_csv_invalid_delimiter,
      parse_file_appl IMPORTING has_header   TYPE abap_bool DEFAULT abap_false
                                path         TYPE string
                      EXPORTING target_table TYPE STANDARD TABLE
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
      parse_file_local IMPORTING has_header   TYPE abap_bool DEFAULT abap_false
                                 path         TYPE string
                       EXPORTING target_table TYPE STANDARD TABLE
                       RAISING   zcx_wd_csv_gui_upload_failed
                                 cx_sy_conversion_error
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_parameter_invalid_type
                                 cx_sy_struct_creation
                                 RESUMABLE(zcx_wd_csv_too_many_columns)
                                 RESUMABLE(zcx_wd_csv_too_few_columns)
                                 RESUMABLE(zcx_wd_csv_mixed_endofline),
      generate_file_appl IMPORTING with_header  TYPE abap_bool DEFAULT abap_false
                                   source_table TYPE STANDARD TABLE
                                   path         TYPE string
                         RAISING   cx_parameter_invalid_range
                                   cx_sy_codepage_converter_init
                                   cx_sy_conversion_codepage
                                   cx_parameter_invalid_type
                                   cx_sy_file_open
                                   cx_sy_file_authority
                                   cx_sy_file_io
                                   cx_sy_file_open_mode
                                   cx_sy_file_close,
      generate_file_local IMPORTING with_header  TYPE abap_bool DEFAULT abap_false
                                    source_table TYPE STANDARD TABLE
                                    path         TYPE string
                          RAISING   zcx_wd_csv_gui_download_failed
                                    cx_parameter_invalid_range
                                    cx_sy_codepage_converter_init
                                    cx_sy_conversion_codepage
                                    cx_parameter_invalid_type,
      decode_xstring IMPORTING encoded TYPE xstring
                     EXPORTING decoded TYPE string
                     RAISING   cx_parameter_invalid_range
                               cx_sy_codepage_converter_init
                               cx_sy_conversion_codepage
                               cx_parameter_invalid_type,
      read_file_appl IMPORTING path       TYPE string
                     EXPORTING csv_string TYPE string
                     RAISING   cx_sy_file_open
                               cx_sy_codepage_converter_init
                               cx_sy_conversion_codepage
                               cx_sy_file_authority
                               cx_sy_file_io
                               cx_sy_file_open_mode
                               cx_sy_file_close,
      read_file_local IMPORTING path       TYPE string
                      EXPORTING csv_string TYPE string
                      RAISING   zcx_wd_csv_gui_upload_failed
                                cx_parameter_invalid_range
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_parameter_invalid_type,
      encode_string IMPORTING decoded TYPE string
                    EXPORTING encoded TYPE xstring
                    RAISING   cx_parameter_invalid_range
                              cx_sy_codepage_converter_init
                              cx_sy_conversion_codepage
                              cx_parameter_invalid_type,
      write_file_appl IMPORTING path       TYPE string
                                csv_string TYPE string
                      RAISING   cx_parameter_invalid_range
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_parameter_invalid_type
                                cx_sy_file_open
                                cx_sy_file_authority
                                cx_sy_file_io
                                cx_sy_file_open_mode
                                cx_sy_file_close,
      write_file_local IMPORTING path       TYPE string
                                 csv_string TYPE string
                       RAISING   zcx_wd_csv_gui_download_failed
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_sy_conversion_codepage
                                 cx_parameter_invalid_type.
  PROTECTED SECTION.
    DATA:
      encoding    TYPE abap_encod,
      replacement TYPE abap_repl,
      ignore_cerr TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv_file IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    super->constructor( endofline   = endofline
                        separator   = separator
                        delimiter   = delimiter
                        conv_exit   = conv_exit
                        trim_spaces = trim_spaces ).

* ---------------------------------------------------------------------
    me->encoding    = encoding.
    me->replacement = replacement.
    me->ignore_cerr = ignore_cerr.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD decode_xstring.
* ---------------------------------------------------------------------
    cl_abap_conv_in_ce=>create( encoding    = encoding
                                replacement = replacement
                                ignore_cerr = ignore_cerr )->convert( EXPORTING input = encoded
                                                                      IMPORTING data  = decoded ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD encode_string.
* ---------------------------------------------------------------------
    cl_abap_conv_out_ce=>create( encoding    = encoding
                                 replacement = replacement
                                 ignore_cerr = ignore_cerr )->convert( EXPORTING data   = decoded
                                                                       IMPORTING buffer = encoded ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_file_appl.
* ---------------------------------------------------------------------
    DATA:
      csv_string TYPE string.

* ---------------------------------------------------------------------
    generate_string( EXPORTING with_header  = with_header
                               source_table = source_table
                     IMPORTING csv_string   = csv_string  ).

* ---------------------------------------------------------------------
    write_file_appl( path       = path
                     csv_string = csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_file_local.
* ---------------------------------------------------------------------
    DATA:
      csv_string TYPE string.

* ---------------------------------------------------------------------
    generate_string( EXPORTING with_header  = with_header
                               source_table = source_table
                     IMPORTING csv_string   = csv_string  ).

* ---------------------------------------------------------------------
    write_file_local( path       = path
                      csv_string = csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_appl.
* ---------------------------------------------------------------------
    DATA:
      csv_string TYPE string.

* ---------------------------------------------------------------------
    read_file_appl( EXPORTING path = path
                    IMPORTING csv_string = csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING has_header = has_header
                            csv_string = csv_string
                  IMPORTING target_table = target_table ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_file_local.
* ---------------------------------------------------------------------
    DATA:
      csv_string TYPE string.

* ---------------------------------------------------------------------
    read_file_local( EXPORTING path       = path
                     IMPORTING csv_string = csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING has_header = has_header
                            csv_string = csv_string
                  IMPORTING target_table = target_table       ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_appl.
* ---------------------------------------------------------------------
    DATA:
      csv_xstring TYPE xstring,
      message     TYPE string.

* ---------------------------------------------------------------------
    OPEN DATASET path FOR INPUT IN BINARY MODE MESSAGE message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid    = cx_sy_file_io=>read_error
          filename  = cl_fs_path=>create( name = path )->get_file_name( )
          errorcode = -1
          errortext = message.
    ENDIF.

* ---------------------------------------------------------------------
    READ DATASET path INTO csv_xstring.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid   = cx_sy_file_io=>cx_sy_file_access_error
          filename = cl_fs_path=>create( name = path )->get_file_name( ).
    ENDIF.

* ---------------------------------------------------------------------
    CLOSE DATASET path.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid   = cx_sy_file_io=>cx_sy_file_access_error
          filename = cl_fs_path=>create( name = path )->get_file_name( ).
    ENDIF.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING encoded = csv_xstring
                    IMPORTING decoded = csv_string  ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_local.
* ---------------------------------------------------------------------
    DATA:
      filelength    TYPE i,
      file_contents TYPE solix_tab,
      csv_xstring   TYPE xstring.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = path
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = filelength
                                          CHANGING   data_tab   = file_contents
                                          EXCEPTIONS OTHERS     = 1             ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_csv_gui_upload_failed.
    ENDIF.

* ---------------------------------------------------------------------
    csv_xstring = cl_bcs_convert=>solix_to_xstring( it_solix = file_contents
                                                    iv_size  = filelength ).

* ---------------------------------------------------------------------
    " free up some memory
    FREE file_contents.

* ---------------------------------------------------------------------
    decode_xstring( EXPORTING encoded = csv_xstring
                    IMPORTING decoded = csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD write_file_appl.
* ---------------------------------------------------------------------
    DATA:
      csv_xstring TYPE xstring,
      message     TYPE string.

* ---------------------------------------------------------------------
    encode_string( EXPORTING decoded = csv_string
                   IMPORTING encoded = csv_xstring ).

* ---------------------------------------------------------------------
    OPEN DATASET path FOR OUTPUT IN BINARY MODE MESSAGE message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid    = cx_sy_file_io=>write_error
          filename  = cl_fs_path=>create( name = path )->get_file_name( )
          errorcode = -1
          errortext = message.
    ENDIF.

* ---------------------------------------------------------------------
    TRANSFER csv_xstring TO path.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid   = cx_sy_file_io=>cx_sy_file_access_error
          filename = cl_fs_path=>create( name = path )->get_file_name( ).
    ENDIF.

* ---------------------------------------------------------------------
    CLOSE DATASET path.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_io
        EXPORTING
          textid   = cx_sy_file_io=>cx_sy_file_access_error
          filename = cl_fs_path=>create( name = path )->get_file_name( ).
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD write_file_local.
* ---------------------------------------------------------------------
    DATA:
      csv_xstring   TYPE xstring,
      file_contents TYPE solix_tab,
      filelength    TYPE i.

* ---------------------------------------------------------------------
    encode_string( EXPORTING decoded  = csv_string
                   IMPORTING encoded = csv_xstring ).

* ---------------------------------------------------------------------
    filelength = xstrlen( csv_xstring ).
    file_contents = cl_bcs_convert=>xstring_to_solix( csv_xstring ).

* ---------------------------------------------------------------------
    FREE csv_xstring.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = filelength
                                                       filename     = path
                                                       filetype     = 'BIN'
                                            CHANGING   data_tab     = file_contents
                                            EXCEPTIONS OTHERS       = 1              ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_csv_gui_download_failed.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
