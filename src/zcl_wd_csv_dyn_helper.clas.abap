CLASS zcl_wd_csv_dyn_helper DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_name_mapping,
        csv  TYPE string,
        abap TYPE string,
      END OF ty_name_mapping,
      ty_name_mappings TYPE STANDARD TABLE OF ty_name_mapping WITH DEFAULT KEY.
    CONSTANTS:
      c_guess_char_count TYPE i VALUE 100000.
    CLASS-METHODS:
      guess_endofline_4str IMPORTING csv_string       TYPE string
                                     guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                           RETURNING VALUE(result)    TYPE string,
      guess_endofline_4appl_file IMPORTING path             TYPE string
                                           encoding         TYPE abap_encod DEFAULT '4110'
                                           guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                 RETURNING VALUE(result)    TYPE string
                                 RAISING   cx_sy_file_open
                                           cx_sy_codepage_converter_init
                                           cx_sy_conversion_codepage
                                           cx_sy_file_authority
                                           cx_sy_file_io
                                           cx_sy_file_open_mode
                                           cx_sy_file_close,
      guess_endofline_4lcl_file IMPORTING path             TYPE string
                                          encoding         TYPE abap_encod DEFAULT '4110'
                                          guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                RETURNING VALUE(result)    TYPE string
                                RAISING   zcx_wd_csv_gui_upload_failed
                                          cx_parameter_invalid_range
                                          cx_sy_codepage_converter_init
                                          cx_sy_conversion_codepage
                                          cx_parameter_invalid_type,
      guess_separator_4str IMPORTING csv_string       TYPE string
                                     guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                           RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_separator,
      guess_separator_4appl_file IMPORTING path             TYPE string
                                           encoding         TYPE abap_encod DEFAULT '4110'
                                           guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                 RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_separator
                                 RAISING   cx_sy_file_open
                                           cx_sy_codepage_converter_init
                                           cx_sy_conversion_codepage
                                           cx_sy_file_authority
                                           cx_sy_file_io
                                           cx_sy_file_open_mode
                                           cx_sy_file_close,
      guess_separator_4lcl_file IMPORTING path             TYPE string
                                          encoding         TYPE abap_encod DEFAULT '4110'
                                          guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_separator
                                RAISING   zcx_wd_csv_gui_upload_failed
                                          cx_parameter_invalid_range
                                          cx_sy_codepage_converter_init
                                          cx_sy_conversion_codepage
                                          cx_parameter_invalid_type,
      guess_delimiter_4str IMPORTING csv_string       TYPE string
                                     guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                           RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_delimiter,
      guess_delimiter_4appl_file IMPORTING path             TYPE string
                                           encoding         TYPE abap_encod DEFAULT '4110'
                                           guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                 RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_delimiter
                                 RAISING   cx_sy_file_open
                                           cx_sy_codepage_converter_init
                                           cx_sy_conversion_codepage
                                           cx_sy_file_authority
                                           cx_sy_file_io
                                           cx_sy_file_open_mode
                                           cx_sy_file_close,
      guess_delimiter_4lcl_file IMPORTING path             TYPE string
                                          encoding         TYPE abap_encod DEFAULT '4110'
                                          guess_char_count TYPE i          DEFAULT zcl_wd_csv_dyn_helper=>c_guess_char_count
                                RETURNING VALUE(result)    TYPE zcl_wd_csv=>ty_delimiter
                                RAISING   zcx_wd_csv_gui_upload_failed
                                          cx_parameter_invalid_range
                                          cx_sy_codepage_converter_init
                                          cx_sy_conversion_codepage
                                          cx_parameter_invalid_type,
      generate_struct_type_4str IMPORTING csv_string              TYPE string
                                          endofline               TYPE csequence                DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                          separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                          delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                          use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                          name_mappings           TYPE ty_name_mappings OPTIONAL
                                RETURNING VALUE(result)           TYPE REF TO cl_abap_structdescr
                                RAISING   zcx_wd_csv_invalid_endofline
                                          zcx_wd_csv_invalid_separator
                                          zcx_wd_csv_invalid_delimiter
                                          RESUMABLE(zcx_wd_csv_mixed_endofline)
                                          cx_sy_conversion_error
                                          cx_sy_range_out_of_bounds
                                          cx_sy_struct_creation
                                          cx_sy_table_creation,
      generate_struct_type_4appl_fil IMPORTING path                    TYPE string
                                               encoding                TYPE abap_encod               DEFAULT '4110'
                                               endofline               TYPE csequence                DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                               separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                               delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                               use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                               name_mappings           TYPE ty_name_mappings OPTIONAL
                                     RETURNING VALUE(result)           TYPE REF TO cl_abap_structdescr
                                     RAISING   zcx_wd_csv_invalid_endofline
                                               zcx_wd_csv_invalid_separator
                                               zcx_wd_csv_invalid_delimiter
                                               RESUMABLE(zcx_wd_csv_mixed_endofline)
                                               cx_sy_conversion_error
                                               cx_sy_range_out_of_bounds
                                               cx_sy_struct_creation
                                               cx_sy_table_creation,
      generate_struct_type_4lcl_file IMPORTING path                    TYPE string
                                               encoding                TYPE abap_encod               DEFAULT '4110'
                                               endofline               TYPE csequence                DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                               separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                               delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                               use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                               name_mappings           TYPE ty_name_mappings OPTIONAL
                                     RETURNING VALUE(result)           TYPE REF TO cl_abap_structdescr
                                     RAISING   zcx_wd_csv_invalid_endofline
                                               zcx_wd_csv_invalid_separator
                                               zcx_wd_csv_invalid_delimiter
                                               RESUMABLE(zcx_wd_csv_mixed_endofline)
                                               cx_sy_conversion_error
                                               cx_sy_range_out_of_bounds
                                               cx_sy_struct_creation
                                               cx_sy_table_creation
                                               zcx_wd_csv_gui_upload_failed
                                               cx_parameter_invalid_range
                                               cx_sy_codepage_converter_init
                                               cx_parameter_invalid_type,
      generate_table_type_4str IMPORTING csv_string              TYPE string
                                         endofline               TYPE csequence DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                         separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                         delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                         use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                         name_mappings           TYPE ty_name_mappings OPTIONAL
                               RETURNING VALUE(result)           TYPE REF TO cl_abap_tabledescr
                               RAISING   zcx_wd_csv_invalid_endofline
                                         zcx_wd_csv_invalid_separator
                                         zcx_wd_csv_invalid_delimiter
                                         RESUMABLE(zcx_wd_csv_mixed_endofline)
                                         cx_sy_range_out_of_bounds
                                         cx_sy_conversion_error
                                         cx_sy_struct_creation
                                         cx_sy_table_creation,
      generate_table_type_4appl_file IMPORTING path                    TYPE string
                                               encoding                TYPE abap_encod               DEFAULT '4110'
                                               endofline               TYPE csequence                DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                               separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                               delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                               use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                               name_mappings           TYPE ty_name_mappings OPTIONAL
                                     RETURNING VALUE(result)           TYPE REF TO cl_abap_tabledescr
                                     RAISING   zcx_wd_csv_invalid_endofline
                                               zcx_wd_csv_invalid_separator
                                               zcx_wd_csv_invalid_delimiter
                                               RESUMABLE(zcx_wd_csv_mixed_endofline)
                                               cx_sy_range_out_of_bounds
                                               cx_sy_conversion_error
                                               cx_sy_struct_creation
                                               cx_sy_table_creation,
      generate_table_type_4lcl_file IMPORTING path                    TYPE string
                                              encoding                TYPE abap_encod               DEFAULT '4110'
                                              endofline               TYPE csequence                DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                                              separator               TYPE zcl_wd_csv=>ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                                              delimiter               TYPE zcl_wd_csv=>ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                                              use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                              name_mappings           TYPE ty_name_mappings OPTIONAL
                                    RETURNING VALUE(result)           TYPE REF TO cl_abap_tabledescr
                                    RAISING   zcx_wd_csv_invalid_endofline
                                              zcx_wd_csv_invalid_separator
                                              zcx_wd_csv_invalid_delimiter
                                              RESUMABLE(zcx_wd_csv_mixed_endofline)
                                              cx_sy_range_out_of_bounds
                                              cx_sy_conversion_error
                                              cx_sy_struct_creation
                                              cx_sy_table_creation
                                              zcx_wd_csv_gui_upload_failed
                                              cx_parameter_invalid_range
                                              cx_sy_codepage_converter_init
                                              cx_parameter_invalid_type.

  PROTECTED SECTION.
    CLASS-METHODS:
      user_header_as_comp_name IMPORTING endofline     TYPE csequence
                                         separator     TYPE zcl_wd_csv=>ty_separator
                                         delimiter     TYPE zcl_wd_csv=>ty_delimiter
                                         struct_descr  TYPE REF TO cl_abap_structdescr
                                         components    TYPE cl_abap_structdescr=>component_table
                                         first_line    TYPE string
                                         name_mappings TYPE ty_name_mappings OPTIONAL
                               RETURNING VALUE(result) TYPE REF TO cl_abap_structdescr
                               RAISING   zcx_wd_csv_invalid_endofline
                                         zcx_wd_csv_invalid_separator
                                         zcx_wd_csv_invalid_delimiter
                                         zcx_wd_csv_mixed_endofline
                                         cx_sy_table_creation
                                         cx_sy_struct_creation
                                         cx_sy_conversion_error
                                         cx_sy_range_out_of_bounds,
      get_column_name IMPORTING col_index     TYPE i
                      RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv_dyn_helper IMPLEMENTATION.



  METHOD guess_endofline_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the end of line character based on the number of occurences
    " this will NOT work for delimited end of line characters!
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF ty_eol_count,
        eol   TYPE string,
        count TYPE i,
      END OF ty_eol_count,
      ty_eol_counts TYPE STANDARD TABLE OF ty_eol_count WITH DEFAULT KEY.
    DATA:
      eol_counts TYPE ty_eol_counts.

* ----------------------------------------------------------------------
    IF strlen( csv_string ) > guess_char_count.
      DATA(l_csv_string) = csv_string(guess_char_count).
    ELSE.
      l_csv_string = csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    eol_counts = VALUE #( ( eol = zcl_wd_csv=>c_endofline_cr_lf
                            count = count( val = l_csv_string
                                           sub = zcl_wd_csv=>c_endofline_cr_lf ) )
                          ( eol = zcl_wd_csv=>c_endofline_lf
                            count = count( val = l_csv_string
                                           sub = zcl_wd_csv=>c_endofline_lf ) )
                          ( eol = zcl_wd_csv=>c_endofline_cr
                            count = count( val = l_csv_string
                                           sub = zcl_wd_csv=>c_endofline_cr ) ) ).

* ----------------------------------------------------------------------
    LOOP AT eol_counts ASSIGNING FIELD-SYMBOL(<eol_count>).
      IF <eol_count>-count <> 0.
        DATA(lv_eol_guessed) = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_eol_guessed = abap_false.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
    IF  eol_counts[ 1 ]-count = eol_counts[ 2 ]-count
    AND eol_counts[ 1 ]-count = eol_counts[ 3 ]-count.
      " all line ending chars occur the same number of times
      " -> end of line is probably carriage return and linefeed
      result = zcl_wd_csv=>c_endofline_cr_lf.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
    " return eol char with most occurences
    " this will return the wrong eol if any delimited eol chars exist
    SORT eol_counts BY count DESCENDING.
    result = eol_counts[ 1 ]-eol.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_delimiter_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the delimiter character based on the number of occurences
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF ty_delimiter_count,
        delimiter TYPE string,
        count     TYPE i,
      END OF ty_delimiter_count,
      ty_delimiter_counts TYPE STANDARD TABLE OF ty_delimiter_count WITH DEFAULT KEY.
    DATA:
      delimiter_count TYPE ty_delimiter_counts.

* ----------------------------------------------------------------------
    IF strlen( csv_string ) > guess_char_count.
      DATA(l_csv_string) = csv_string(guess_char_count).
    ELSE.
      l_csv_string = csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    delimiter_count = VALUE #( ( delimiter = zcl_wd_csv=>c_delimiter_single_quote
                                 count = count( val = l_csv_string
                                                sub = zcl_wd_csv=>c_delimiter_single_quote ) )
                               ( delimiter = zcl_wd_csv=>c_delimiter_double_quote
                                 count = count( val = l_csv_string
                                                sub = zcl_wd_csv=>c_delimiter_double_quote ) ) ).

* ----------------------------------------------------------------------
    LOOP AT delimiter_count ASSIGNING FIELD-SYMBOL(<del_count>).
      IF <del_count>-count <> 0.
        DATA(del_guessed) = abap_true.
      ENDIF.
    ENDLOOP.
    IF del_guessed = abap_false.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
    SORT delimiter_count BY count DESCENDING.
    result = delimiter_count[ 1 ]-delimiter.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_separator_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the separator character based on the number of occurences
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF ty_separator_count,
        separator TYPE string,
        count     TYPE i,
      END OF ty_separator_count,
      ty_separator_counts TYPE STANDARD TABLE OF ty_separator_count WITH DEFAULT KEY.
    DATA:
      separator_counts TYPE ty_separator_counts.

* ----------------------------------------------------------------------
    IF strlen( csv_string ) > guess_char_count.
      DATA(l_csv_string) = csv_string(guess_char_count).
    ELSE.
      l_csv_string = csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    separator_counts = VALUE #( ( separator = zcl_wd_csv=>c_separator_comma
                                  count = count( val = l_csv_string
                                                 sub = zcl_wd_csv=>c_separator_comma ) )
                                ( separator = zcl_wd_csv=>c_separator_semicolon
                                  count = count( val = l_csv_string
                                                 sub = zcl_wd_csv=>c_separator_semicolon ) )
                                ( separator = zcl_wd_csv=>c_separator_tab
                                  count = count( val = l_csv_string
                                                 sub = zcl_wd_csv=>c_separator_tab ) ) ).

* ----------------------------------------------------------------------
    LOOP AT separator_counts ASSIGNING FIELD-SYMBOL(<sep_count>).
      IF <sep_count>-count <> 0.
        DATA(sep_guessed) = abap_true.
      ENDIF.
    ENDLOOP.
    IF sep_guessed = abap_false.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
    SORT separator_counts BY count DESCENDING.
    result = separator_counts[ 1 ]-separator.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_struct_type_4str.
* ----------------------------------------------------------------------
    " reuse input validation of zcl_wd_csv
    NEW zcl_wd_csv( endofline   = endofline
                    separator   = separator
                    delimiter   = delimiter
                    conv_exit   = abap_false
                    trim_spaces = abap_false ).

* ----------------------------------------------------------------------
    DATA:
      str_length     TYPE i,
      str_pos        TYPE i,
      str_pos_p1     TYPE i,
      curr_line      TYPE i,
      component      TYPE i,
      delimited      TYPE abap_bool,
      in_cell        TYPE abap_bool,
      first_line     TYPE string,
      len_first_line TYPE i,
      components     TYPE cl_abap_structdescr=>component_table.

* ---------------------------------------------------------------------
    DEFINE continue_loop.
**********************************************************************
      str_pos = str_pos + 1.
      CONTINUE.
**********************************************************************
    END-OF-DEFINITION.

* ---------------------------------------------------------------------
    DO.
      CASE csv_string+str_pos(1).
        WHEN delimiter.
          CASE delimited.
            WHEN abap_false.
              delimited = abap_true.
            WHEN abap_true.
              IF ( str_length - str_pos ) >= 2 " make sure at least two characters are left in the string
              AND csv_string+str_pos(2) = delimiter && delimiter.
                " if the current csv cell is delimited and double double quotes are in it, add one of them to the abap cell
*                append_character.
                str_pos = str_pos + 1.
                continue_loop.
              ELSE.
                delimited = abap_false.
              ENDIF.
          ENDCASE.
        WHEN separator.
          IF delimited = abap_true.
            continue_loop.
          ENDIF.
          in_cell = abap_false.
          component = component + 1.
        WHEN zcl_wd_csv=>c_endofline_lf OR zcl_wd_csv=>c_endofline_cr_lf(1).
          IF delimited = abap_true.
            continue_loop.
          ENDIF.
          IF (     endofline = zcl_wd_csv=>c_endofline_cr_lf
               AND csv_string+str_pos(2) <> zcl_wd_csv=>c_endofline_cr_lf )
          OR (     endofline = zcl_wd_csv=>c_endofline_lf
               AND csv_string+str_pos(1) <> zcl_wd_csv=>c_endofline_lf    )
          OR (     endofline = zcl_wd_csv=>c_endofline_cr
               AND csv_string+str_pos(1) <> zcl_wd_csv=>c_endofline_cr    ).
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_mixed_endofline
              EXPORTING
                line = curr_line.
          ENDIF.
          CASE endofline.
            WHEN zcl_wd_csv=>c_endofline_cr OR zcl_wd_csv=>c_endofline_lf.
              str_pos_p1 = str_pos + 1.
            WHEN zcl_wd_csv=>c_endofline_cr_lf ##WHEN_DOUBLE_OK.
              str_pos_p1 = str_pos + 2.
          ENDCASE.
          IF csv_string+str_pos_p1 CO space.
            EXIT.
          ENDIF.
          IF endofline = zcl_wd_csv=>c_endofline_cr_lf.
            " advance position because crlf is two characters
            str_pos = str_pos + 1.
          ENDIF.
          IF use_header_as_comp_name = abap_true
          AND curr_line = 0.
            len_first_line = str_pos + 1.
            first_line = csv_string(len_first_line).
          ENDIF.
          curr_line = curr_line + 1.
          IF curr_line = 3.
            " parse max two lines
            EXIT.
          ENDIF.
          component = 1.
        WHEN ` `.
          IF delimited = abap_true
          OR in_cell   = abap_true.
            continue_loop.
          ENDIF.
        WHEN OTHERS.
          in_cell = abap_true.
      ENDCASE.
      IF ( str_pos + 1 ) = str_length.
        EXIT.
      ENDIF.
      str_pos = str_pos + 1.
    ENDDO.

* ----------------------------------------------------------------------
    DO component TIMES.
      APPEND VALUE #( name = get_column_name( sy-index )
                      type = cl_abap_elemdescr=>get_string( )
      ) TO components.
    ENDDO.
    result = cl_abap_structdescr=>create( components ).

* ----------------------------------------------------------------------
    IF use_header_as_comp_name = abap_false.
      RETURN.
    ENDIF.
    result = user_header_as_comp_name( endofline     = endofline
                                       delimiter     = delimiter
                                       separator     = separator
                                       struct_descr  = result
                                       components    = components
                                       first_line    = first_line
                                       name_mappings = name_mappings ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_table_type_4str.
* ----------------------------------------------------------------------
    result = cl_abap_tabledescr=>create( p_line_type  = generate_struct_type_4str( csv_string              = csv_string
                                                                                   endofline               = endofline
                                                                                   separator               = separator
                                                                                   delimiter               = delimiter
                                                                                   use_header_as_comp_name = use_header_as_comp_name
                                                                                   name_mappings           = name_mappings )
                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD user_header_as_comp_name.
* ----------------------------------------------------------------------
    DATA:
      table        TYPE REF TO data,
      l_components LIKE components.
    FIELD-SYMBOLS:
      <table> TYPE STANDARD TABLE.

* ----------------------------------------------------------------------
    DATA(table_descr) = cl_abap_tabledescr=>create( struct_descr ).
    CREATE DATA table TYPE HANDLE table_descr.
    ASSIGN table->* TO <table>.

* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv( endofline   = endofline
                        separator   = separator
                        delimiter   = delimiter
                        conv_exit   = abap_false
                        trim_spaces = abap_true    )->parse_string( EXPORTING has_header = abap_false
                                                                              csv_string = first_line
                                                                    IMPORTING target_table = <table> ).
      CATCH zcx_wd_csv_too_many_columns
            zcx_wd_csv_too_few_columns.
        " these errors can probably be ignored, because the nr of cols was determined earlier
    ENDTRY.

* ----------------------------------------------------------------------
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
        ASSIGN COMPONENT <component>-name OF STRUCTURE <line> TO FIELD-SYMBOL(<col_name>).
        TRY.
            DATA(abap_col_name) = name_mappings[ csv = to_upper( <col_name> ) ]-abap.
          CATCH cx_sy_itab_line_not_found.
            abap_col_name = <col_name>.
        ENDTRY.
        abap_col_name = to_upper( abap_col_name ).
        APPEND VALUE #( name = abap_col_name
                        type = <component>-type
        ) TO l_components.
      ENDLOOP.
      EXIT.
    ENDLOOP.

* ----------------------------------------------------------------------
    result = cl_abap_structdescr=>create( l_components ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_column_name.
* ----------------------------------------------------------------------
    result = |COL_{ col_index }|.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_struct_type_4appl_fil.
* ----------------------------------------------------------------------
    NEW zcl_wd_csv_file( encoding    = encoding
                         replacement = '#'
                         ignore_cerr = abap_true   )->read_file_appl( EXPORTING path       = path
                                                                      IMPORTING csv_string = DATA(csv_string) ).

* ----------------------------------------------------------------------
    result = generate_struct_type_4str( csv_string              = csv_string
                                        endofline               = endofline
                                        separator               = separator
                                        delimiter               = delimiter
                                        use_header_as_comp_name = use_header_as_comp_name
                                        name_mappings           = name_mappings ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_struct_type_4lcl_file.
* ----------------------------------------------------------------------
    NEW zcl_wd_csv_file( encoding    = encoding
                         replacement = '#'
                         ignore_cerr = abap_true   )->read_file_local( EXPORTING path       = path
                                                                       IMPORTING csv_string = DATA(csv_string) ).

* ----------------------------------------------------------------------
    result = generate_struct_type_4str( csv_string              = csv_string
                                        endofline               = endofline
                                        separator               = separator
                                        delimiter               = delimiter
                                        use_header_as_comp_name = use_header_as_comp_name
                                        name_mappings           = name_mappings ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_table_type_4appl_file.
* ----------------------------------------------------------------------
    NEW zcl_wd_csv_file( encoding    = encoding
                         replacement = '#'
                         ignore_cerr = abap_true   )->read_file_appl( EXPORTING path       = path
                                                                      IMPORTING csv_string = DATA(csv_string) ).

* ----------------------------------------------------------------------
    result = generate_table_type_4str( csv_string              = csv_string
                                       endofline               = endofline
                                       separator               = separator
                                       delimiter               = delimiter
                                       use_header_as_comp_name = use_header_as_comp_name
                                       name_mappings           = name_mappings ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_table_type_4lcl_file.
* ----------------------------------------------------------------------
    NEW zcl_wd_csv_file( encoding    = encoding
                         replacement = '#'
                         ignore_cerr = abap_true   )->read_file_local( EXPORTING path       = path
                                                                       IMPORTING csv_string = DATA(csv_string) ).

* ----------------------------------------------------------------------
    result = generate_table_type_4str( csv_string              = csv_string
                                       endofline               = endofline
                                       separator               = separator
                                       delimiter               = delimiter
                                       use_header_as_comp_name = use_header_as_comp_name
                                       name_mappings            = name_mappings ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_delimiter_4appl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_appl( EXPORTING path       = path
                                                                          IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.

* ----------------------------------------------------------------------
    result = guess_delimiter_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_delimiter_4lcl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_local( EXPORTING path       = path
                                                                           IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.


* ----------------------------------------------------------------------
    result = guess_delimiter_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_endofline_4appl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_appl( EXPORTING path       = path
                                                                          IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.

* ----------------------------------------------------------------------
    result = guess_endofline_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_endofline_4lcl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_local( EXPORTING path       = path
                                                                           IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.

* ----------------------------------------------------------------------
    result = guess_endofline_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_separator_4appl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_appl( EXPORTING path       = path
                                                                          IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.

* ----------------------------------------------------------------------
    result = guess_separator_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_separator_4lcl_file.
* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv_file( encoding    = encoding
                             replacement = '#'
                             ignore_cerr = abap_true   )->read_file_local( EXPORTING path       = path
                                                                           IMPORTING csv_string = DATA(csv_string) ).
      CATCH zcx_wd_csv_invalid_delimiter zcx_wd_csv_invalid_endofline zcx_wd_csv_invalid_separator.
    ENDTRY.

* ----------------------------------------------------------------------
    result = guess_separator_4str( csv_string       = csv_string
                                   guess_char_count = guess_char_count ).

* ----------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
