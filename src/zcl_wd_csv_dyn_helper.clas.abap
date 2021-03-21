CLASS zcl_wd_csv_dyn_helper DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF mty_s_name_mapping,
        csv  TYPE string,
        abap TYPE string,
      END OF mty_s_name_mapping,
      mty_t_name_mapping TYPE STANDARD TABLE OF mty_s_name_mapping WITH DEFAULT KEY.
    CONSTANTS:
      mc_guess_char_count TYPE i VALUE 100000.
    CLASS-METHODS:
      guess_endofline_4str IMPORTING iv_csv_string       TYPE string
                                     iv_guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>mc_guess_char_count
                           RETURNING VALUE(rv_endofline) TYPE string,
      guess_separator_4str IMPORTING iv_csv_string       TYPE string
                                     iv_guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>mc_guess_char_count
                           RETURNING VALUE(rv_separator) TYPE zcl_wd_csv=>mty_separator,
      guess_delimiter_4str IMPORTING iv_csv_string       TYPE string
                                     iv_guess_char_count TYPE i DEFAULT zcl_wd_csv_dyn_helper=>mc_guess_char_count
                           RETURNING VALUE(rv_delimiter) TYPE zcl_wd_csv=>mty_delimiter,
      generate_struct_type_4str IMPORTING iv_csv_string              TYPE string
                                          iv_endofline               TYPE csequence                 DEFAULT zcl_wd_csv=>mc_endofline_cr_lf
                                          iv_separator               TYPE zcl_wd_csv=>mty_separator DEFAULT zcl_wd_csv=>mc_separator_tab
                                          iv_delimiter               TYPE zcl_wd_csv=>mty_delimiter DEFAULT zcl_wd_csv=>mc_delimiter_double_quote
                                          iv_use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                          it_name_mapping            TYPE mty_t_name_mapping OPTIONAL
                                RETURNING VALUE(ro_struct_descr)     TYPE REF TO cl_abap_structdescr
                                RAISING   zcx_wd_csv_invalid_endofline
                                          zcx_wd_csv_invalid_separator
                                          zcx_wd_csv_invalid_delimiter
                                          RESUMABLE(zcx_wd_csv_mixed_endofline)
                                          cx_sy_conversion_error
                                          cx_sy_range_out_of_bounds
                                          cx_sy_struct_creation
                                          cx_sy_table_creation,
      generate_table_type_4str IMPORTING iv_csv_string              TYPE string
                                         iv_endofline               TYPE csequence                 DEFAULT zcl_wd_csv=>mc_endofline_cr_lf
                                         iv_separator               TYPE zcl_wd_csv=>mty_separator DEFAULT zcl_wd_csv=>mc_separator_tab
                                         iv_delimiter               TYPE zcl_wd_csv=>mty_delimiter DEFAULT zcl_wd_csv=>mc_delimiter_double_quote
                                         iv_use_header_as_comp_name TYPE abap_bool DEFAULT abap_false
                                         it_name_mapping            TYPE mty_t_name_mapping OPTIONAL
                               RETURNING VALUE(ro_table_descr)      TYPE REF TO cl_abap_tabledescr
                               RAISING   zcx_wd_csv_invalid_endofline
                                         zcx_wd_csv_invalid_separator
                                         zcx_wd_csv_invalid_delimiter
                                         RESUMABLE(zcx_wd_csv_mixed_endofline)
                                         cx_sy_range_out_of_bounds
                                         cx_sy_conversion_error
                                         cx_sy_struct_creation
                                         cx_sy_table_creation.
  PROTECTED SECTION.
    CLASS-METHODS:
      user_header_as_comp_name IMPORTING iv_endofline           TYPE csequence
                                         iv_separator           TYPE zcl_wd_csv=>mty_separator
                                         iv_delimiter           TYPE zcl_wd_csv=>mty_delimiter
                                         io_struct_descr        TYPE REF TO cl_abap_structdescr
                                         it_components          TYPE cl_abap_structdescr=>component_table
                                         iv_first_line          TYPE string
                                         it_name_mapping        TYPE mty_t_name_mapping OPTIONAL
                               RETURNING VALUE(ro_struct_descr) TYPE REF TO cl_abap_structdescr
                               RAISING   zcx_wd_csv_invalid_endofline
                                         zcx_wd_csv_invalid_separator
                                         zcx_wd_csv_invalid_delimiter
                                         zcx_wd_csv_mixed_endofline
                                         cx_sy_table_creation
                                         cx_sy_struct_creation
                                         cx_sy_conversion_error
                                         cx_sy_range_out_of_bounds,
      get_column_name IMPORTING iv_col_index       TYPE i
                      RETURNING VALUE(rv_col_name) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv_dyn_helper IMPLEMENTATION.



  METHOD guess_endofline_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the end of line character based on the number of occurences
    " this will NOT work for delimited end of line characters!
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF lty_s_eol_count,
        eol   TYPE string,
        count TYPE i,
      END OF lty_s_eol_count,
      lty_t_eol_count TYPE STANDARD TABLE OF lty_s_eol_count WITH DEFAULT KEY.
    DATA:
      lt_eol_count TYPE lty_t_eol_count.

* ----------------------------------------------------------------------
    IF strlen( iv_csv_string ) > iv_guess_char_count.
      DATA(lv_csv_string) = iv_csv_string(iv_guess_char_count).
    ELSE.
      lv_csv_string = iv_csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    APPEND VALUE #( eol = zcl_wd_csv=>mc_endofline_cr_lf
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_endofline_cr_lf )
    ) TO lt_eol_count.
    APPEND VALUE #( eol = zcl_wd_csv=>mc_endofline_lf
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_endofline_lf )
    ) TO lt_eol_count.
    APPEND VALUE #( eol = zcl_wd_csv=>mc_endofline_cr
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_endofline_cr )
    ) TO lt_eol_count.

* ----------------------------------------------------------------------
    IF  lt_eol_count[ 1 ]-count = lt_eol_count[ 2 ]-count
    AND lt_eol_count[ 1 ]-count = lt_eol_count[ 3 ]-count.
      " all line ending chars occur the same number of times
      " -> end of line is probably carriage return and linefeed
      rv_endofline = zcl_wd_csv=>mc_endofline_cr_lf.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
    " return eol char with most occurences
    " this will return the wrong eol if any delimited eol chars exist
    SORT lt_eol_count BY count DESCENDING.
    rv_endofline = lt_eol_count[ 1 ]-eol.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_delimiter_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the delimiter character based on the number of occurences
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF lty_s_delimiter_count,
        delimiter TYPE string,
        count     TYPE i,
      END OF lty_s_delimiter_count,
      lty_t_delimiter_count TYPE STANDARD TABLE OF lty_s_delimiter_count WITH DEFAULT KEY.
    DATA:
      lt_delimiter_count TYPE lty_t_delimiter_count.

* ----------------------------------------------------------------------
    IF strlen( iv_csv_string ) > iv_guess_char_count.
      DATA(lv_csv_string) = iv_csv_string(iv_guess_char_count).
    ELSE.
      lv_csv_string = iv_csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    APPEND VALUE #( delimiter = zcl_wd_csv=>mc_delimiter_single_quote
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_delimiter_single_quote )
    ) TO lt_delimiter_count.
    APPEND VALUE #( delimiter = zcl_wd_csv=>mc_delimiter_double_quote
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_delimiter_double_quote )
    ) TO lt_delimiter_count.

* ----------------------------------------------------------------------
    SORT lt_delimiter_count BY count DESCENDING.
    rv_delimiter = lt_delimiter_count[ 1 ]-delimiter.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD guess_separator_4str.
* ----------------------------------------------------------------------
    " this method tries to guess the separator character based on the number of occurences
* ----------------------------------------------------------------------
    TYPES:
      BEGIN OF lty_s_separator_count,
        separator TYPE string,
        count     TYPE i,
      END OF lty_s_separator_count,
      lty_t_separator_count TYPE STANDARD TABLE OF lty_s_separator_count WITH DEFAULT KEY.
    DATA:
      lt_separator_count TYPE lty_t_separator_count.

* ----------------------------------------------------------------------
    IF strlen( iv_csv_string ) > iv_guess_char_count.
      DATA(lv_csv_string) = iv_csv_string(iv_guess_char_count).
    ELSE.
      lv_csv_string = iv_csv_string.
    ENDIF.

* ----------------------------------------------------------------------
    APPEND VALUE #( separator = zcl_wd_csv=>mc_separator_comma
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_separator_comma )
    ) TO lt_separator_count.
    APPEND VALUE #( separator = zcl_wd_csv=>mc_separator_semicolon
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_separator_semicolon )
    ) TO lt_separator_count.
    APPEND VALUE #( separator = zcl_wd_csv=>mc_separator_tab
                    count = count( val = lv_csv_string
                                   sub = zcl_wd_csv=>mc_separator_tab )
    ) TO lt_separator_count.

* ----------------------------------------------------------------------
    SORT lt_separator_count BY count DESCENDING.
    rv_separator = lt_separator_count[ 1 ]-separator.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_struct_type_4str.
* ----------------------------------------------------------------------
    " reuse input validation of zcl_wd_csv
    NEW zcl_wd_csv( iv_endofline   = iv_endofline
                    iv_separator   = iv_separator
                    iv_delimiter   = iv_delimiter
                    iv_conv_exit   = abap_false
                    iv_trim_spaces = abap_false ).

* ----------------------------------------------------------------------
    DATA:
      lv_str_length     TYPE i,
      lv_str_pos        TYPE i,
      lv_str_pos_p1     TYPE i,
      lv_curr_line      TYPE i,
      lv_component      TYPE i,
      lv_delimited      TYPE abap_bool,
      lv_in_cell        TYPE abap_bool,
      lv_first_line     TYPE string,
      lv_len_first_line TYPE i,
      lt_components     TYPE cl_abap_structdescr=>component_table.

* ---------------------------------------------------------------------
    DEFINE continue_loop.
**********************************************************************
      lv_str_pos = lv_str_pos + 1.
      CONTINUE.
**********************************************************************
    END-OF-DEFINITION.

* ---------------------------------------------------------------------
    DO.
      CASE iv_csv_string+lv_str_pos(1).
        WHEN iv_delimiter.
          CASE lv_delimited.
            WHEN abap_false.
              lv_delimited = abap_true.
            WHEN abap_true.
              IF ( lv_str_length - lv_str_pos ) >= 2 " make sure at least two characters are left in the string
              AND iv_csv_string+lv_str_pos(2) = iv_delimiter && iv_delimiter.
                " if the current csv cell is delimited and double double quotes are in it, add one of them to the abap cell
*                append_character.
                lv_str_pos = lv_str_pos + 1.
                continue_loop.
              ELSE.
                lv_delimited = abap_false.
              ENDIF.
          ENDCASE.
        WHEN iv_separator.
          IF lv_delimited = abap_true.
            continue_loop.
          ENDIF.
          lv_in_cell = abap_false.
          lv_component = lv_component + 1.
        WHEN zcl_wd_csv=>mc_endofline_lf OR zcl_wd_csv=>mc_endofline_cr_lf(1).
          IF lv_delimited = abap_true.
            continue_loop.
          ENDIF.
          IF (     iv_endofline = zcl_wd_csv=>mc_endofline_cr_lf
               AND iv_csv_string+lv_str_pos(2) <> zcl_wd_csv=>mc_endofline_cr_lf )
          OR (     iv_endofline = zcl_wd_csv=>mc_endofline_lf
               AND iv_csv_string+lv_str_pos(1) <> zcl_wd_csv=>mc_endofline_lf    )
          OR (     iv_endofline = zcl_wd_csv=>mc_endofline_cr
               AND iv_csv_string+lv_str_pos(1) <> zcl_wd_csv=>mc_endofline_cr    ).
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_mixed_endofline
              EXPORTING
                line = lv_curr_line.
          ENDIF.
          CASE iv_endofline.
            WHEN zcl_wd_csv=>mc_endofline_cr OR zcl_wd_csv=>mc_endofline_lf.
              lv_str_pos_p1 = lv_str_pos + 1.
            WHEN zcl_wd_csv=>mc_endofline_cr_lf ##WHEN_DOUBLE_OK.
              lv_str_pos_p1 = lv_str_pos + 2.
          ENDCASE.
          IF iv_csv_string+lv_str_pos_p1 CO space.
            EXIT.
          ENDIF.
          IF iv_endofline = zcl_wd_csv=>mc_endofline_cr_lf.
            " advance position because crlf is two characters
            lv_str_pos = lv_str_pos + 1.
          ENDIF.
          IF iv_use_header_as_comp_name = abap_true
          AND lv_curr_line = 0.
            lv_len_first_line = lv_str_pos + 1.
            lv_first_line = iv_csv_string(lv_len_first_line).
          ENDIF.
          lv_curr_line = lv_curr_line + 1.
          IF lv_curr_line = 3.
            " parse max two lines
            EXIT.
          ENDIF.
          lv_component = 1.
        WHEN ` `.
          IF lv_delimited = abap_true
          OR lv_in_cell   = abap_true.
            continue_loop.
          ENDIF.
        WHEN OTHERS.
          lv_in_cell = abap_true.
      ENDCASE.
      IF ( lv_str_pos + 1 ) = lv_str_length.
        EXIT.
      ENDIF.
      lv_str_pos = lv_str_pos + 1.
    ENDDO.

* ----------------------------------------------------------------------
    DO lv_component TIMES.
      APPEND VALUE #( name = get_column_name( sy-index )
                      type = cl_abap_elemdescr=>get_string( )
      ) TO lt_components.
    ENDDO.
    ro_struct_descr = cl_abap_structdescr=>create( lt_components ).

* ----------------------------------------------------------------------
    IF iv_use_header_as_comp_name = abap_false.
      RETURN.
    ENDIF.
    ro_struct_descr = user_header_as_comp_name( iv_endofline    = iv_endofline
                                                iv_delimiter    = iv_delimiter
                                                iv_separator    = iv_separator
                                                io_struct_descr = ro_struct_descr
                                                it_components   = lt_components
                                                iv_first_line   = lv_first_line
                                                it_name_mapping = it_name_mapping ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_table_type_4str.
* ----------------------------------------------------------------------
    ro_table_descr = cl_abap_tabledescr=>create( p_line_type  = generate_struct_type_4str( iv_csv_string              = iv_csv_string
                                                                                           iv_endofline               = iv_endofline
                                                                                           iv_separator               = iv_separator
                                                                                           iv_delimiter               = iv_delimiter
                                                                                           iv_use_header_as_comp_name = iv_use_header_as_comp_name
                                                                                           it_name_mapping            = it_name_mapping )
                                                 p_table_kind = cl_abap_tabledescr=>tablekind_std ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD user_header_as_comp_name.
* ----------------------------------------------------------------------
    DATA:
      ld_table      TYPE REF TO data,
      lt_components LIKE it_components.
    FIELD-SYMBOLS:
      <lt_table> TYPE STANDARD TABLE.

* ----------------------------------------------------------------------
    DATA(lo_table_descr) = cl_abap_tabledescr=>create( io_struct_descr ).
    CREATE DATA ld_table TYPE HANDLE lo_table_descr.
    ASSIGN ld_table->* TO <lt_table>.

* ----------------------------------------------------------------------
    TRY.
        NEW zcl_wd_csv( iv_endofline   = iv_endofline
                        iv_separator   = iv_separator
                        iv_delimiter   = iv_delimiter
                        iv_conv_exit   = abap_false
                        iv_trim_spaces = abap_true    )->parse_string( EXPORTING iv_has_header = abap_false
                                                                                 iv_csv_string = iv_first_line
                                                                       IMPORTING et_data       = <lt_table> ).
      CATCH zcx_wd_csv_too_many_columns
            zcx_wd_csv_too_few_columns.
        " these errors can probably be ignored, because the nr of cols was determined earlier
    ENDTRY.

* ----------------------------------------------------------------------
    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls>).
      LOOP AT it_components ASSIGNING FIELD-SYMBOL(<ls_component>).
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls> TO FIELD-SYMBOL(<lv_col_name>).
        TRY.
            DATA(lv_abap_col_name) = it_name_mapping[ csv = to_upper( <lv_col_name> ) ]-abap.
          CATCH cx_sy_itab_line_not_found.
            lv_abap_col_name = <lv_col_name>.
        ENDTRY.
        lv_abap_col_name = to_upper( lv_abap_col_name ).
        APPEND VALUE #( name = lv_abap_col_name
                        type = <ls_component>-type
        ) TO lt_components.
      ENDLOOP.
      EXIT.
    ENDLOOP.

* ----------------------------------------------------------------------
    ro_struct_descr = cl_abap_structdescr=>create( lt_components ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_column_name.
* ----------------------------------------------------------------------
    rv_col_name = |COL_{ iv_col_index }|.

* ----------------------------------------------------------------------
  ENDMETHOD.


ENDCLASS.
