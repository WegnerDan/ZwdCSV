CLASS zcl_wd_csv DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      ty_separator TYPE c LENGTH 1,
      ty_delimiter TYPE c LENGTH 1,
      BEGIN OF ty_header_column,
        index TYPE i,
        name  TYPE string,
      END OF ty_header_column,
      ty_header_columns TYPE SORTED TABLE OF ty_header_column WITH UNIQUE KEY index.
    CONSTANTS:
      c_separator_tab          TYPE ty_separator VALUE cl_abap_char_utilities=>horizontal_tab,
      c_separator_semicolon    TYPE ty_separator VALUE ';',
      c_separator_comma        TYPE ty_separator VALUE ',',
      c_delimiter_single_quote TYPE ty_delimiter VALUE '''',
      c_delimiter_double_quote TYPE ty_delimiter VALUE '"',
      c_endofline_lf           TYPE c LENGTH 1    VALUE cl_abap_char_utilities=>newline,
      c_endofline_cr_lf        TYPE c LENGTH 2    VALUE cl_abap_char_utilities=>cr_lf,
      c_endofline_cr           TYPE c LENGTH 1    VALUE cl_abap_char_utilities=>cr_lf.
    METHODS:
      constructor IMPORTING iv_endofline   TYPE csequence    DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                            iv_separator   TYPE ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                            iv_delimiter   TYPE ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                            iv_conv_exit   TYPE abap_bool    DEFAULT abap_false
                            iv_trim_spaces TYPE abap_bool    DEFAULT abap_false
                  RAISING   zcx_wd_csv_invalid_endofline
                            zcx_wd_csv_invalid_separator
                            zcx_wd_csv_invalid_delimiter,
      parse_string IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                             iv_csv_string TYPE string
                   EXPORTING et_data       TYPE STANDARD TABLE
                   RAISING   cx_sy_struct_creation
                             cx_sy_conversion_error
                             cx_sy_range_out_of_bounds
                             RESUMABLE(zcx_wd_csv_too_many_columns)
                             RESUMABLE(zcx_wd_csv_too_few_columns)
                             RESUMABLE(zcx_wd_csv_mixed_endofline),
      generate_string IMPORTING iv_with_header TYPE abap_bool DEFAULT abap_false
                                it_data        TYPE STANDARD TABLE
                      EXPORTING ev_csv_string  TYPE string,
      get_header_columns RETURNING VALUE(rt_header_columns) TYPE ty_header_columns,
      get_separator RETURNING VALUE(rv_separator) TYPE ty_separator,
      set_separator IMPORTING iv_separator TYPE ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                    RAISING   zcx_wd_csv_invalid_separator,
      get_endofline RETURNING VALUE(rv_endofline) TYPE string,
      set_endofline IMPORTING iv_endofline TYPE csequence DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                    RAISING   zcx_wd_csv_invalid_endofline,
      get_delimiter RETURNING VALUE(rv_delimiter) TYPE ty_delimiter,
      set_delimiter IMPORTING iv_delimiter TYPE ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                    RAISING   zcx_wd_csv_invalid_delimiter,
      get_conv_exit RETURNING VALUE(rv_conv_exit) TYPE abap_bool,
      set_conv_exit IMPORTING iv_conv_exit TYPE abap_bool DEFAULT abap_true,
      get_trim_spaces RETURNING VALUE(result) TYPE abap_bool,
      set_trim_spaces IMPORTING iv_trim_spaces TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_string_struc,
        ref     TYPE REF TO data,
        columns TYPE i,
      END OF ty_string_struc,
      BEGIN OF ty_comp_conversion_exit,
        name     TYPE string,
        convexit TYPE convexit,
        temp_fld TYPE REF TO data,
      END OF ty_comp_conversion_exit,
      ty_comp_conversion_exits TYPE SORTED TABLE OF ty_comp_conversion_exit WITH UNIQUE KEY name.
    DATA:
      endofline           TYPE string, " length can be 1 or 2 characters.
      separator           TYPE ty_separator,
      delimiter           TYPE ty_delimiter,
      conv_exit           TYPE abap_bool,
      trim_spaces_enabled TYPE abap_bool,
      ts_parse            TYPE timestampl,
      ts_convex           TYPE timestampl,
      comp_conv_exits     TYPE ty_comp_conversion_exits,
      header_columns      TYPE ty_header_columns.
    METHODS:
      create_string_struc IMPORTING it_data             TYPE ANY TABLE
                          RETURNING VALUE(rs_str_struc) TYPE ty_string_struc
                          RAISING   cx_sy_struct_creation,
      fill_header_columns_tab IMPORTING is_header TYPE any,
      move_data IMPORTING iv_conv_exit   TYPE abap_bool
                          iv_trim_spaces TYPE abap_bool
                CHANGING  cs_str         TYPE any
                          cs_exp         TYPE any,
      call_conv_exits IMPORTING is TYPE any
                      CHANGING  cs TYPE any,
      trim_spaces CHANGING cs TYPE any,
      generate_cell IMPORTING iv_fieldname   TYPE string
                              iv_fieldtype   TYPE REF TO cl_abap_datadescr
                              iv_data        TYPE any
                              iv_conv_exit   TYPE abap_bool
                              iv_trim_spaces TYPE abap_bool
                    RETURNING VALUE(rv_cell) TYPE string,
      contains_only_empty_lines IMPORTING iv        TYPE any
                                RETURNING VALUE(rv) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv IMPLEMENTATION.


  METHOD call_conv_exits.
* ---------------------------------------------------------------------
    DATA:
      lo_structdescr TYPE REF TO cl_abap_structdescr,
      lv_conv_exit   TYPE funcname,
      lr             TYPE REF TO data.
    FIELD-SYMBOLS:
      <lv_temp> TYPE any,
      <lv_str>  TYPE any,
      <lv_exp>  TYPE any.

* ---------------------------------------------------------------------
    IF ts_convex <> ts_parse.
      ts_convex = ts_parse.
      FREE comp_conv_exits.
      lo_structdescr ?= cl_abap_structdescr=>describe_by_data( cs ).
      LOOP AT lo_structdescr->get_included_view( ) ASSIGNING FIELD-SYMBOL(<ls_component>).
*
        CAST cl_abap_elemdescr( <ls_component>-type )->get_ddic_field( RECEIVING  p_flddescr = DATA(ls_dfies)
                                                                       EXCEPTIONS OTHERS     = 1              ).
        IF sy-subrc <> 0
        OR ls_dfies-convexit IS INITIAL.
          CONTINUE.
        ENDIF.

        CREATE DATA lr TYPE HANDLE <ls_component>-type.
        INSERT VALUE #( name     = <ls_component>-name
                        convexit = ls_dfies-convexit
                        temp_fld = lr                  ) INTO TABLE comp_conv_exits.
      ENDLOOP.
    ENDIF.

* ---------------------------------------------------------------------
    LOOP AT comp_conv_exits ASSIGNING FIELD-SYMBOL(<ls>).
      ASSIGN <ls>-temp_fld->* TO <lv_temp>.
      FREE <lv_temp>.
      ASSIGN COMPONENT <ls>-name OF STRUCTURE is TO <lv_str>.
      ASSIGN COMPONENT <ls>-name OF STRUCTURE cs TO <lv_exp>.
      lv_conv_exit = 'CONVERSION_EXIT_' && <ls>-convexit && '_INPUT'.
      CALL FUNCTION lv_conv_exit
        EXPORTING
          input  = <lv_str>
        IMPORTING
          output = <lv_temp>
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc = 0.
        <lv_exp> = <lv_temp>.
      ENDIF.
    ENDLOOP.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD constructor.
* ---------------------------------------------------------------------
    set_endofline( iv_endofline ).

* ---------------------------------------------------------------------
    set_separator( iv_separator ).

* ---------------------------------------------------------------------
    set_delimiter( iv_delimiter ).

* ---------------------------------------------------------------------
    set_conv_exit( iv_conv_exit ).

* ---------------------------------------------------------------------
    set_trim_spaces( iv_trim_spaces ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD contains_only_empty_lines.
* ---------------------------------------------------------------------
    CASE strlen( endofline ).
      WHEN 1.
        IF iv CO ` ` && endofline.
          rv = abap_true.
        ENDIF.
      WHEN 2.
        " CO alone is not sufficient in case of 2 character end of line
        " first check if only end of line and space are contained for speed (CO does not care about the order of the characters)
        " then check if a the string without end of line characters contains only spaces
        " only if both are true, return true
        IF iv CO ` ` && endofline
        AND replace( val  = iv
                     with = space
                     occ  = 0
                     sub  = endofline ) CO ` `.
          rv = abap_true.
        ENDIF.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD create_string_struc.
* ---------------------------------------------------------------------
    " create structure with string components and the same component names
    " as the line type of the export table
* ---------------------------------------------------------------------
    DATA:
      lo_tabledescr     TYPE REF TO cl_abap_tabledescr,
      lo_structdescr    TYPE REF TO cl_abap_structdescr,
      lt_components     TYPE abap_component_view_tab,
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
    rs_str_struc-columns = lines( lt_components_str ).
    lo_structdescr = cl_abap_structdescr=>create( lt_components_str ).
    CREATE DATA rs_str_struc-ref TYPE HANDLE lo_structdescr.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD fill_header_columns_tab.
* ---------------------------------------------------------------------
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_header TO FIELD-SYMBOL(<lv>).
      IF sy-subrc <> 0. RETURN. ENDIF.
      INSERT VALUE #( index = sy-index
                      name  = <lv>
                    ) INTO TABLE header_columns.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_cell.
* ---------------------------------------------------------------------
    DATA:
      lv_cell    TYPE c LENGTH 200, " randomly selected, should be enough right?
      lv_delimit TYPE abap_bool.

* ---------------------------------------------------------------------
    CASE iv_conv_exit.
      WHEN abap_true.
        WRITE iv_data TO lv_cell LEFT-JUSTIFIED.
        rv_cell = lv_cell.
      WHEN abap_false.
        rv_cell = iv_data.
    ENDCASE.

* ---------------------------------------------------------------------
    IF iv_trim_spaces = abap_true.
      CONDENSE rv_cell.
    ENDIF.

* ---------------------------------------------------------------------
    " escape quotes
    IF find( val = rv_cell sub = delimiter ) >= 0.
      lv_delimit = abap_true.
      rv_cell = replace( val  = rv_cell
                         sub  = delimiter
                         occ  = 0
                         with = delimiter && delimiter ).
    ENDIF.

* ---------------------------------------------------------------------
    " if the cell contains a separator or any newline character, it needs to be delimited
    IF lv_delimit = abap_false
    AND (    find( val = rv_cell sub = separator                          ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf      ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf+0(1) ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf+1(1) ) >= 0 ).
      lv_delimit = abap_true.
    ENDIF.

* ---------------------------------------------------------------------
    IF lv_delimit = abap_true.
      rv_cell = delimiter && rv_cell && delimiter.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_string.
* ---------------------------------------------------------------------
    DATA:
      lo_tabledescr    TYPE REF TO cl_abap_tabledescr,
      lo_structdescr   TYPE REF TO cl_abap_structdescr,
      lt_components    TYPE abap_component_view_tab,
      lv_line          TYPE string,
      lt_lines         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      lv_start_newline TYPE abap_bool.
    FIELD-SYMBOLS:
      <ls_component> TYPE abap_simple_componentdescr,
      <ls_data>      TYPE any,
      <lv_data>      TYPE data.

* ---------------------------------------------------------------------
    FREE: ev_csv_string, header_columns.

* ---------------------------------------------------------------------
    lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    lt_components = lo_structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    IF iv_with_header = abap_true.
      LOOP AT lt_components ASSIGNING <ls_component>.
        INSERT VALUE #( index = sy-tabix
                        name  = <ls_component>-name
                      ) INTO TABLE header_columns.

        IF lv_line IS INITIAL.
          lv_line = generate_cell( iv_fieldname   = <ls_component>-name
                                   iv_fieldtype   = <ls_component>-type
                                   iv_data        = <ls_component>-name
                                   iv_conv_exit   = conv_exit
                                   iv_trim_spaces = trim_spaces_enabled ).
        ELSE.
             lv_line = lv_line && separator && generate_cell( iv_fieldname   = <ls_component>-name
                                                              iv_fieldtype   = <ls_component>-type
                                                              iv_data        = <ls_component>-name
                                                              iv_conv_exit   = conv_exit
                                                              iv_trim_spaces = trim_spaces_enabled ).
        ENDIF.
      ENDLOOP.
      lv_line = lv_line && endofline.
      APPEND lv_line TO lt_lines.
    ENDIF.

* ---------------------------------------------------------------------
    lv_start_newline = abap_true.

* ---------------------------------------------------------------------
    LOOP AT it_data ASSIGNING <ls_data>.
      FREE lv_line.
      LOOP AT lt_components ASSIGNING <ls_component>.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_data> TO <lv_data>.
        IF lv_line IS INITIAL.
          lv_line = generate_cell( iv_fieldname   = <ls_component>-name
                                   iv_fieldtype   = <ls_component>-type
                                   iv_data        = <lv_data>
                                   iv_conv_exit   = conv_exit
                                   iv_trim_spaces = trim_spaces_enabled ).
        ELSE.
          lv_line =  lv_line
                  && separator
                  && generate_cell( iv_fieldname   = <ls_component>-name
                                    iv_fieldtype   = <ls_component>-type
                                    iv_data        = <lv_data>
                                    iv_conv_exit   = conv_exit
                                    iv_trim_spaces = trim_spaces_enabled ).
        ENDIF.
      ENDLOOP.
      lv_line = lv_line && endofline.
      APPEND lv_line TO lt_lines.
    ENDLOOP.

* ---------------------------------------------------------------------
    ev_csv_string = concat_lines_of( lt_lines ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_conv_exit.
* ---------------------------------------------------------------------
    rv_conv_exit = conv_exit.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_delimiter.
* ---------------------------------------------------------------------
    rv_delimiter = delimiter.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_endofline.
* ---------------------------------------------------------------------
    rv_endofline = endofline.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_header_columns.
* ---------------------------------------------------------------------
    rt_header_columns = header_columns.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_separator.
* ---------------------------------------------------------------------
    rv_separator = separator.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_trim_spaces.
* ---------------------------------------------------------------------
    result = trim_spaces_enabled.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD move_data.
* ---------------------------------------------------------------------
    MOVE-CORRESPONDING cs_str TO cs_exp.
    IF iv_trim_spaces = abap_true.
      trim_spaces( CHANGING cs = cs_exp ).
    ENDIF.
    IF iv_conv_exit = abap_true.
      call_conv_exits( EXPORTING is = cs_str
                       CHANGING  cs = cs_exp ).
    ENDIF.
    FREE cs_str.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_string.
* ---------------------------------------------------------------------
    DATA:
      lv_str_length TYPE i,
      lv_str_pos    TYPE i,
      lv_str_pos_p1 TYPE i,
      lv_curr_line  TYPE i,
      lv_component  TYPE i,
      lv_first_line TYPE abap_bool VALUE abap_true,
      lv_delimited  TYPE abap_bool,
      lv_in_cell    TYPE abap_bool,
      ls_str_struc  TYPE ty_string_struc.
    FIELD-SYMBOLS:
      <ls_data_str> TYPE any,  " temporary structure with string types components
      <ls_data_exp> TYPE any,  " line of export table
      <lv_data>     TYPE data. " character

    DEFINE append_line.
**********************************************************************
      APPEND INITIAL LINE TO et_data ASSIGNING <ls_data_exp>.
      IF iv_has_header = abap_true.
        lv_curr_line = sy-tabix + 1.
      ELSE.
        lv_curr_line = sy-tabix.
      ENDIF.
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

* ---------------------------------------------------------------------
    FREE: et_data, header_columns.

* ---------------------------------------------------------------------
    " for conv exit buffering
    GET TIME STAMP FIELD ts_parse.

* ---------------------------------------------------------------------
    ls_str_struc = create_string_struc( et_data ).
    ASSIGN ls_str_struc-ref->* TO <ls_data_str>.

* ---------------------------------------------------------------------
    lv_str_length = strlen( iv_csv_string ).

* ---------------------------------------------------------------------
    " first line
    append_line.

* ---------------------------------------------------------------------
    DO.
      CASE iv_csv_string+lv_str_pos(1).
        WHEN delimiter.
          CASE lv_delimited.
            WHEN abap_false.
              lv_delimited = abap_true.
            WHEN abap_true.
              IF ( lv_str_length - lv_str_pos ) >= 2 " make sure at least two characters are left in the string
              AND iv_csv_string+lv_str_pos(2) = delimiter && delimiter.
                " if the current csv cell is delimited and double double quotes are in it, add one of them to the abap cell
                append_character.
                lv_str_pos = lv_str_pos + 1.
                continue_loop.
              ELSE.
                lv_delimited = abap_false.
              ENDIF.
          ENDCASE.
        WHEN separator.
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          lv_in_cell = abap_false.
          lv_component = lv_component + 1.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
          IF sy-subrc <> 0.
            IF lv_first_line = abap_true.
              lv_curr_line = 1.
            ENDIF.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_many_columns
              EXPORTING
                line = lv_curr_line.
          ENDIF.
        WHEN c_endofline_lf OR c_endofline_cr_lf(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          IF  lv_first_line = abap_true
          AND iv_has_header = abap_true.
            fill_header_columns_tab( <ls_data_str> ).
            lv_first_line = abap_false.
            FREE <ls_data_str>.
            IF lv_component < ls_str_struc-columns.
              RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                EXPORTING
                  line = 1.
            ENDIF.
            lv_component = 1.
            ASSIGN COMPONENT lv_component OF STRUCTURE <ls_data_str> TO <lv_data>.
            IF endofline = c_endofline_cr_lf.
              lv_str_pos = lv_str_pos + 1.
            ENDIF.
            continue_loop.
          ENDIF.
          IF (     endofline = c_endofline_cr_lf
               AND iv_csv_string+lv_str_pos(2) <> c_endofline_cr_lf )
          OR (     endofline = c_endofline_lf
               AND iv_csv_string+lv_str_pos(1) <> c_endofline_lf    )
          OR (     endofline = c_endofline_cr
               AND iv_csv_string+lv_str_pos(1) <> c_endofline_cr    ).
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_mixed_endofline
              EXPORTING
                line = lv_curr_line.
          ENDIF.
          " check if rest of string is empty and parsing is finished
          CASE endofline.
            WHEN c_endofline_cr OR c_endofline_lf.
              lv_str_pos_p1 = lv_str_pos + 1.
            WHEN c_endofline_cr_lf ##WHEN_DOUBLE_OK.
              lv_str_pos_p1 = lv_str_pos + 2.
          ENDCASE.
          IF iv_csv_string+lv_str_pos_p1 CO space.
            IF lv_component < ls_str_struc-columns.
              RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                EXPORTING
                  line = lv_curr_line.
            ENDIF.
            move_data( EXPORTING iv_conv_exit   = conv_exit
                                 iv_trim_spaces = trim_spaces_enabled
                       CHANGING  cs_str = <ls_data_str>
                                 cs_exp = <ls_data_exp>          ).
            EXIT.
            " Without && '' syntax check complains:
            " "Offsets or lengths cannot be specified for fields of type "STRING" or "XSTRING" in the current statement"
          ELSEIF contains_only_empty_lines( iv_csv_string+lv_str_pos_p1 && '' ).
            move_data( EXPORTING iv_conv_exit   = conv_exit
                                 iv_trim_spaces = trim_spaces_enabled
                       CHANGING  cs_str = <ls_data_str>
                                 cs_exp = <ls_data_exp>          ).
            EXIT.
          ENDIF.
          " rest of string not empty (not only spaces/endofline chars)
          IF lv_component < ls_str_struc-columns.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
              EXPORTING
                line = lv_curr_line.
          ENDIF.
          move_data( EXPORTING iv_conv_exit   = conv_exit
                               iv_trim_spaces = trim_spaces_enabled
                     CHANGING  cs_str = <ls_data_str>
                               cs_exp = <ls_data_exp>          ).
          append_line.
          IF endofline = c_endofline_cr_lf.
            " advance position because crlf is two characters
            lv_str_pos = lv_str_pos + 1.
          ENDIF.
        WHEN ` `.
          IF lv_delimited = abap_true
          OR lv_in_cell   = abap_true.
            append_character. continue_loop.
          ELSE.
            IF trim_spaces_enabled = abap_true.
              " ignore space if not currently in cell or delimited
              continue_loop.
            ELSE.
              append_character.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          lv_in_cell = abap_true.
          append_character.
      ENDCASE.
      IF ( lv_str_pos + 1 ) = lv_str_length.
        IF lv_component < ls_str_struc-columns.
          RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
            EXPORTING
              line = lv_curr_line.
        ENDIF.
        move_data( EXPORTING iv_conv_exit   = conv_exit
                             iv_trim_spaces = trim_spaces_enabled
                   CHANGING  cs_str = <ls_data_str>
                             cs_exp = <ls_data_exp>          ).
        EXIT.
      ENDIF.
      lv_str_pos = lv_str_pos + 1.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_conv_exit.
* ---------------------------------------------------------------------
    CASE iv_conv_exit.
      WHEN abap_false.
        conv_exit = abap_false.
      WHEN OTHERS.
        conv_exit = abap_true.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_delimiter.
* ---------------------------------------------------------------------
    IF iv_delimiter = ''''
    OR iv_delimiter = '"'.
      delimiter = iv_delimiter.
    ELSE.
      RAISE EXCEPTION TYPE zcx_wd_csv_invalid_delimiter
        EXPORTING
          delimiter = iv_delimiter.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_endofline.
* ---------------------------------------------------------------------
    " endofline can either be a linefeed/carriage return (one char)
    " or carriage return and linefeed (two chars)
    CASE iv_endofline.
      WHEN c_endofline_lf OR c_endofline_cr_lf OR c_endofline_cr.
        endofline = iv_endofline.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wd_csv_invalid_endofline
          EXPORTING
            end_of_line = iv_endofline.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_separator.
* ---------------------------------------------------------------------
    IF  iv_separator IS NOT INITIAL
    AND iv_separator NA sy-abcde
    AND iv_separator NA '0123456789'.
      separator = iv_separator.
    ELSE.
      RAISE EXCEPTION TYPE zcx_wd_csv_invalid_separator
        EXPORTING
          separator = iv_separator.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_trim_spaces.
* ---------------------------------------------------------------------
    CASE iv_trim_spaces.
      WHEN abap_false.
        trim_spaces_enabled = abap_false.
      WHEN OTHERS.
        trim_spaces_enabled = abap_true.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD trim_spaces.
* ---------------------------------------------------------------------
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE cs TO FIELD-SYMBOL(<lv>).
      IF sy-subrc <> 0. EXIT. ENDIF.
      CONDENSE <lv>.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
