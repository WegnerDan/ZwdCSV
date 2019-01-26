CLASS zcl_wd_csv DEFINITION PUBLIC CREATE PUBLIC.
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
      constructor IMPORTING iv_newline   TYPE mty_newline   DEFAULT mc_default_newline
                            iv_separator TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter TYPE mty_delimiter DEFAULT mc_default_delimiter
                  RAISING   zcx_wd_csv_invalid_newline,
      parse_file_appl IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                                iv_path       TYPE string
                      EXPORTING et_data       TYPE table
                      RAISING   cx_sy_struct_creation,
      parse_file_local IMPORTING iv_has_header  TYPE abap_bool DEFAULT abap_false
                                 iv_path        TYPE string
                                 iv_encoding    TYPE abap_encod DEFAULT '4110'
                                 iv_replacement TYPE abap_repl DEFAULT '#'
                                 iv_ignore_cerr TYPE abap_bool DEFAULT abap_true
                       EXPORTING et_data        TYPE table
                       RAISING   zcx_wd_csv_gui_upload_failed
                                 cx_parameter_invalid_range
                                 cx_sy_codepage_converter_init
                                 cx_sy_conversion_codepage
                                 cx_parameter_invalid_type
                                 cx_sy_struct_creation,
      parse_string IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                             iv_csv_string TYPE string
                   EXPORTING et_data       TYPE table
                   RAISING   cx_sy_struct_creation.
  PROTECTED SECTION.
    DATA:
      mv_separator TYPE mty_separator,
      mv_delimiter TYPE mty_delimiter,
      mv_newline   TYPE mty_newline.
    METHODS:
      create_string_struc IMPORTING it_data             TYPE ANY TABLE
                          RETURNING VALUE(rd_str_struc) TYPE REF TO data
                          RAISING   cx_sy_struct_creation,
      read_file_appl IMPORTING iv_path       TYPE string
                     EXPORTING ev_csv_string TYPE string,
      read_file_local IMPORTING iv_path        TYPE string
                                iv_encoding    TYPE abap_encod
                                iv_replacement TYPE abap_repl
                                iv_ignore_cerr TYPE abap_bool
                      EXPORTING ev_csv_string  TYPE string
                      RAISING   zcx_wd_csv_gui_upload_failed
                                cx_parameter_invalid_range
                                cx_sy_codepage_converter_init
                                cx_sy_conversion_codepage
                                cx_parameter_invalid_type.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    " newline can either be a linefeed or carriage return and linefeed (two chars)
    CASE strlen( iv_newline ).
      WHEN 1 OR 2.
        mv_newline = iv_newline.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wd_csv_invalid_newline.
    ENDCASE.

* ---------------------------------------------------------------------
    mv_separator = iv_separator.
    mv_delimiter = iv_delimiter.

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
    read_file_local( EXPORTING iv_path        = iv_path
                               iv_encoding    = iv_encoding
                               iv_replacement = iv_replacement
                               iv_ignore_cerr = iv_ignore_cerr
                     IMPORTING ev_csv_string  = lv_csv_string ).

* ---------------------------------------------------------------------
    parse_string( EXPORTING iv_has_header = iv_has_header
                            iv_csv_string = lv_csv_string
                  IMPORTING et_data       = et_data       ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_string.
* ---------------------------------------------------------------------
    DATA:
      lv_str_length TYPE i,
      lv_str_pos    TYPE i,
      lv_str_pos_p1 TYPE i,
      lv_component  TYPE i,
      lv_first_line TYPE abap_bool VALUE abap_true,
      lv_delimited  TYPE abap_bool,
      lv_in_cell    TYPE abap_bool,
      ld_str_struc  TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_data_str> TYPE any,  " temporary structure with string types components
      <ls_data_exp> TYPE any,  " line of export table
      <lv_data>     TYPE data. " character

    DEFINE append_line.
**********************************************************************
      APPEND INITIAL LINE TO et_data ASSIGNING <ls_data_exp>.
      lv_component = 1.
      ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
**********************************************************************
    END-OF-DEFINITION.

    DEFINE append_character.
**********************************************************************
      <lv_data> = <lv_data> && iv_csv_string+lv_str_pos(1).
**********************************************************************
    END-OF-DEFINITION.

    DEFINE continue_loop.
**********************************************************************
      lv_str_pos = lv_str_pos + 1.
      CONTINUE.
**********************************************************************
    END-OF-DEFINITION.

    DEFINE move_data.
**********************************************************************
      MOVE-CORRESPONDING <ls_data_str> TO <ls_data_exp>.
      FREE <ls_data_str>.
**********************************************************************
    END-OF-DEFINITION.

* ---------------------------------------------------------------------
    ld_str_struc = create_string_struc( et_data ).
    ASSIGN ld_str_struc->* TO <ls_data_str>.

* ---------------------------------------------------------------------
    lv_str_length = strlen( iv_csv_string ).

* ---------------------------------------------------------------------
    " first line
    append_line.

* ---------------------------------------------------------------------


* ---------------------------------------------------------------------
    DO.
      CASE iv_csv_string+lv_str_pos(1).
        WHEN mv_delimiter.
          CASE lv_delimited.
            WHEN abap_false.
              lv_delimited = abap_true.
            WHEN abap_true.
              IF ( lv_str_length - lv_str_pos ) >= 2 " make sure at least two characters are left in the string
              AND iv_csv_string+lv_str_pos(2) = '""'.
                " if the current csv cell is delimited and double double quotes are in it, add one of them to the abap cell
                append_character.
                lv_str_pos = lv_str_pos + 1.
                continue_loop.
              ELSE.
                lv_delimited = abap_false.
              ENDIF.
          ENDCASE.
        WHEN mv_separator.
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          lv_in_cell = abap_false.
          lv_component = lv_component + 1.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
        WHEN mv_newline(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          CASE strlen( mv_newline ).
            WHEN 1.
              IF lv_first_line  = abap_true
              AND iv_has_header = abap_true.
                lv_first_line = abap_false.
                continue_loop.
              ENDIF.
              lv_str_pos_p1 = lv_str_pos + 1.
              IF iv_csv_string+lv_str_pos_p1 CO space.
                move_data. EXIT.
              ENDIF.
              move_data. append_line.
              lv_first_line = abap_false.
            WHEN 2.
              continue_loop.
            WHEN OTHERS.
          ENDCASE.
        WHEN mv_newline+1(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          IF lv_first_line  = abap_true
          AND iv_has_header = abap_true.
            lv_first_line = abap_false.
            FREE <ls_data_str>.
            lv_component = 1.
            ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
            continue_loop.
          ENDIF.
          lv_str_pos_p1 = lv_str_pos + 1.
          IF iv_csv_string+lv_str_pos_p1 CO space.
            move_data. EXIT.
          ENDIF.
          move_data. append_line.
        WHEN ` `.
          IF lv_delimited = abap_true
          OR lv_in_cell   = abap_true.
            append_character. continue_loop.
          ELSE.
            " ignore space if not currently in cell or delimited
            continue_loop.
          ENDIF.
        WHEN OTHERS.
          lv_in_cell = abap_true.
          append_character.
      ENDCASE.
      IF ( lv_str_pos + 1 ) = lv_str_length.
        move_data.
        EXIT.
      ENDIF.
      lv_str_pos = lv_str_pos + 1.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD read_file_appl.
* ---------------------------------------------------------------------
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
                                          EXCEPTIONS OTHERS     = 19            ).
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
    cl_abap_conv_in_ce=>create( encoding    = iv_encoding
                                replacement = iv_replacement
                                ignore_cerr = iv_ignore_cerr )->convert( EXPORTING input = lv_xstring
                                                                         IMPORTING data  = ev_csv_string ).

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
