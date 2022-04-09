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
      c_endofline_lf           TYPE c LENGTH 1   VALUE cl_abap_char_utilities=>newline,
      c_endofline_cr_lf        TYPE c LENGTH 2   VALUE cl_abap_char_utilities=>cr_lf,
      c_endofline_cr           TYPE c LENGTH 1   VALUE cl_abap_char_utilities=>cr_lf.
    METHODS:
      constructor IMPORTING endofline   TYPE csequence    DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                            separator   TYPE ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                            delimiter   TYPE ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                            conv_exit   TYPE abap_bool    DEFAULT abap_false
                            trim_spaces TYPE abap_bool    DEFAULT abap_false
                  RAISING   zcx_wd_csv_invalid_endofline
                            zcx_wd_csv_invalid_separator
                            zcx_wd_csv_invalid_delimiter,
      parse_string IMPORTING has_header   TYPE abap_bool DEFAULT abap_false
                             csv_string   TYPE string
                   EXPORTING target_table TYPE STANDARD TABLE
                   RAISING   cx_sy_struct_creation
                             cx_sy_conversion_error
                             cx_sy_range_out_of_bounds
                             RESUMABLE(zcx_wd_csv_too_many_columns)
                             RESUMABLE(zcx_wd_csv_too_few_columns)
                             RESUMABLE(zcx_wd_csv_mixed_endofline),
      generate_string IMPORTING with_header  TYPE abap_bool DEFAULT abap_false
                                source_table TYPE STANDARD TABLE
                      EXPORTING csv_string   TYPE string,
      get_header_columns RETURNING VALUE(result) TYPE ty_header_columns,
      get_separator RETURNING VALUE(result) TYPE ty_separator,
      set_separator IMPORTING separator TYPE ty_separator DEFAULT zcl_wd_csv=>c_separator_tab
                    RAISING   zcx_wd_csv_invalid_separator,
      get_endofline RETURNING VALUE(result) TYPE string,
      set_endofline IMPORTING endofline TYPE csequence DEFAULT zcl_wd_csv=>c_endofline_cr_lf
                    RAISING   zcx_wd_csv_invalid_endofline,
      get_delimiter RETURNING VALUE(result) TYPE ty_delimiter,
      set_delimiter IMPORTING delimiter TYPE ty_delimiter DEFAULT zcl_wd_csv=>c_delimiter_double_quote
                    RAISING   zcx_wd_csv_invalid_delimiter,
      get_conv_exit RETURNING VALUE(result) TYPE abap_bool,
      set_conv_exit IMPORTING conv_exit TYPE abap_bool DEFAULT abap_true,
      get_trim_spaces RETURNING VALUE(result) TYPE abap_bool,
      set_trim_spaces IMPORTING trim_spaces TYPE abap_bool DEFAULT abap_true.
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
      create_string_struc IMPORTING target_table  TYPE ANY TABLE
                          RETURNING VALUE(result) TYPE ty_string_struc
                          RAISING   cx_sy_struct_creation,
      fill_header_columns_tab IMPORTING header TYPE any,
      move_data CHANGING source TYPE any
                         target TYPE any,
      call_conv_exits IMPORTING source TYPE any
                      CHANGING  target TYPE any,
      trim_spaces CHANGING target TYPE any,
      generate_cell IMPORTING fieldname     TYPE string
                              fieldtype     TYPE REF TO cl_abap_datadescr
                              value         TYPE any
                    RETURNING VALUE(result) TYPE string,
      contains_only_empty_lines IMPORTING value         TYPE any
                                RETURNING VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv IMPLEMENTATION.


  METHOD call_conv_exits.
* ---------------------------------------------------------------------
    DATA:
      structdescr        TYPE REF TO cl_abap_structdescr,
      conv_exit_funcname TYPE funcname,
      temp_field         TYPE REF TO data.
    FIELD-SYMBOLS:
      <temp_field>  TYPE any,
      <source_comp> TYPE any,
      <target_comp> TYPE any.

* ---------------------------------------------------------------------
    IF ts_convex <> ts_parse.
      ts_convex = ts_parse.
      FREE comp_conv_exits.
      structdescr ?= cl_abap_structdescr=>describe_by_data( target ).
      LOOP AT structdescr->get_included_view( ) ASSIGNING FIELD-SYMBOL(<component>).
        CAST cl_abap_elemdescr( <component>-type )->get_ddic_field( RECEIVING p_flddescr = DATA(field_descr)
                                                                    EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0
        OR field_descr-convexit IS INITIAL.
          CONTINUE.
        ENDIF.

        CREATE DATA temp_field TYPE HANDLE <component>-type.
        INSERT VALUE #( name     = <component>-name
                        convexit = field_descr-convexit
                        temp_fld = temp_field ) INTO TABLE comp_conv_exits.
      ENDLOOP.
    ENDIF.

* ---------------------------------------------------------------------
    LOOP AT comp_conv_exits ASSIGNING FIELD-SYMBOL(<comp_conv_exit>).
      ASSIGN <comp_conv_exit>-temp_fld->* TO <temp_field>.
      FREE <temp_field>.
      ASSIGN COMPONENT <comp_conv_exit>-name OF STRUCTURE source TO <source_comp>.
      ASSIGN COMPONENT <comp_conv_exit>-name OF STRUCTURE target TO <target_comp>.
      conv_exit_funcname = 'CONVERSION_EXIT_' && <comp_conv_exit>-convexit && '_INPUT'.
      CALL FUNCTION conv_exit_funcname
        EXPORTING
          input  = <source_comp>
        IMPORTING
          output = <temp_field>
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc = 0.
        <target_comp> = <temp_field>.
      ENDIF.
    ENDLOOP.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD constructor.
* ---------------------------------------------------------------------
    set_endofline( endofline ).

* ---------------------------------------------------------------------
    set_separator( separator ).

* ---------------------------------------------------------------------
    set_delimiter( delimiter ).

* ---------------------------------------------------------------------
    set_conv_exit( conv_exit ).

* ---------------------------------------------------------------------
    set_trim_spaces( trim_spaces ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD contains_only_empty_lines.
* ---------------------------------------------------------------------
    CASE strlen( endofline ).
      WHEN 1.
        IF value CO ` ` && endofline.
          result = abap_true.
        ENDIF.
      WHEN 2.
        " CO alone is not sufficient in case of 2 character end of line
        " first check if only end of line and space are contained for speed (CO does not care about the order of the characters)
        " then check if a the string without end of line characters contains only spaces
        " only if both are true, return true
        IF value CO ` ` && endofline
        AND replace( val  = value
                     with = space
                     occ  = 0
                     sub  = endofline ) CO ` `.
          result = abap_true.
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
      tabledescr        TYPE REF TO cl_abap_tabledescr,
      structdescr       TYPE REF TO cl_abap_structdescr,
      target_components TYPE abap_component_view_tab,
      string_components TYPE cl_abap_structdescr=>component_table.

* ---------------------------------------------------------------------
    tabledescr ?= cl_abap_typedescr=>describe_by_data( target_table ).
    structdescr ?= tabledescr->get_table_line_type( ).
    target_components = structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    LOOP AT target_components ASSIGNING FIELD-SYMBOL(<target_component>).
      APPEND VALUE #( name = <target_component>-name
                      type = cl_abap_elemdescr=>get_string( ) ) TO string_components.
    ENDLOOP.

* ---------------------------------------------------------------------
    result-columns = lines( string_components ).
    structdescr = cl_abap_structdescr=>create( string_components ).
    CREATE DATA result-ref TYPE HANDLE structdescr.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD fill_header_columns_tab.
* ---------------------------------------------------------------------
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE header TO FIELD-SYMBOL(<header_comp>).
      IF sy-subrc <> 0. RETURN. ENDIF.
      INSERT VALUE #( index = sy-index
                      name  = <header_comp>
                    ) INTO TABLE header_columns.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_cell.
* ---------------------------------------------------------------------
    DATA:
      cell    TYPE c LENGTH 200, " randomly selected, should be enough right?
      delimit TYPE abap_bool.

* ---------------------------------------------------------------------
    CASE conv_exit.
      WHEN abap_true.
        WRITE value TO cell LEFT-JUSTIFIED.
        result = cell.
      WHEN abap_false.
        result = value.
    ENDCASE.

* ---------------------------------------------------------------------
    IF trim_spaces_enabled = abap_true.
      CONDENSE result.
    ENDIF.

* ---------------------------------------------------------------------
    " escape quotes
    IF find( val = result sub = delimiter ) >= 0.
      delimit = abap_true.
      result = replace( val  = result
                        sub  = delimiter
                        occ  = 0
                        with = delimiter && delimiter ).
    ENDIF.

* ---------------------------------------------------------------------
    " if the cell contains a separator or any newline character, it needs to be delimited
    IF delimit = abap_false
    AND (    find( val = result sub = separator                          ) >= 0
          OR find( val = result sub = cl_abap_char_utilities=>cr_lf      ) >= 0
          OR find( val = result sub = cl_abap_char_utilities=>cr_lf+0(1) ) >= 0
          OR find( val = result sub = cl_abap_char_utilities=>cr_lf+1(1) ) >= 0 ).
      delimit = abap_true.
    ENDIF.

* ---------------------------------------------------------------------
    IF delimit = abap_true.
      result = delimiter && result && delimiter.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_string.
* ---------------------------------------------------------------------
    DATA:
      tabledescr    TYPE REF TO cl_abap_tabledescr,
      structdescr   TYPE REF TO cl_abap_structdescr,
      components    TYPE abap_component_view_tab,
      line          TYPE string,
      lines         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      start_newline TYPE abap_bool.
    FIELD-SYMBOLS:
      <component>   TYPE abap_simple_componentdescr,
      <source_line> TYPE any,
      <source_comp> TYPE data.

* ---------------------------------------------------------------------
    FREE: csv_string, header_columns.

* ---------------------------------------------------------------------
    tabledescr ?= cl_abap_typedescr=>describe_by_data( source_table ).
    structdescr ?= tabledescr->get_table_line_type( ).
    components = structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    IF with_header = abap_true.
      LOOP AT components ASSIGNING <component>.
        INSERT VALUE #( index = sy-tabix
                        name  = <component>-name
                      ) INTO TABLE header_columns.

        IF line IS INITIAL.
          line = generate_cell( fieldname = <component>-name
                                fieldtype = <component>-type
                                value      = <component>-name ).
        ELSE.
          line = line && separator && generate_cell( fieldname = <component>-name
                                                     fieldtype = <component>-type
                                                     value      = <component>-name ).
        ENDIF.
      ENDLOOP.
      line = line && endofline.
      APPEND line TO lines.
    ENDIF.

* ---------------------------------------------------------------------
    start_newline = abap_true.

* ---------------------------------------------------------------------
    LOOP AT source_table ASSIGNING <source_line>.
      FREE line.
      LOOP AT components ASSIGNING <component>.
        ASSIGN COMPONENT <component>-name OF STRUCTURE <source_line> TO <source_comp>.
        IF line IS INITIAL.
          line = generate_cell( fieldname = <component>-name
                                fieldtype = <component>-type
                                value      = <source_comp> ).
        ELSE.
          line =  line
                  && separator
                  && generate_cell( fieldname = <component>-name
                                    fieldtype = <component>-type
                                    value      = <source_comp> ).
        ENDIF.
      ENDLOOP.
      line = line && endofline.
      APPEND line TO lines.
    ENDLOOP.

* ---------------------------------------------------------------------
    csv_string = concat_lines_of( lines ).

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_conv_exit.
* ---------------------------------------------------------------------
    result = conv_exit.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_delimiter.
* ---------------------------------------------------------------------
    result = delimiter.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_endofline.
* ---------------------------------------------------------------------
    result = endofline.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_header_columns.
* ---------------------------------------------------------------------
    result = header_columns.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_separator.
* ---------------------------------------------------------------------
    result = separator.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_trim_spaces.
* ---------------------------------------------------------------------
    result = trim_spaces_enabled.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD move_data.
* ---------------------------------------------------------------------
    MOVE-CORRESPONDING source TO target.
    IF trim_spaces_enabled = abap_true.
      trim_spaces( CHANGING target = target ).
    ENDIF.
    IF conv_exit = abap_true.
      call_conv_exits( EXPORTING source = source
                       CHANGING  target = target ).
    ENDIF.
    FREE source.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD parse_string.
* ---------------------------------------------------------------------
    DATA:
      str_length   TYPE i,
      str_pos      TYPE i,
      str_pos_p1   TYPE i,
      curr_line    TYPE i,
      component    TYPE i,
      first_line   TYPE abap_bool VALUE abap_true,
      delimited    TYPE abap_bool,
      in_cell      TYPE abap_bool,
      string_struc TYPE ty_string_struc.
    FIELD-SYMBOLS:
      <string_struc> TYPE any,  " temporary structure with string types components
      <target_line>  TYPE any,  " line of export table
      <string_comp>  TYPE data. " character

    DEFINE append_line.
**********************************************************************
      APPEND INITIAL LINE TO target_table ASSIGNING <target_line>.
      IF has_header = abap_true.
        curr_line = sy-tabix + 1.
      ELSE.
        curr_line = sy-tabix.
      ENDIF.
      component = 1.
      ASSIGN COMPONENT component OF STRUCTURE <string_struc> TO <string_comp>.
**********************************************************************
    END-OF-DEFINITION.

    DEFINE append_character.
**********************************************************************
      <string_comp> = <string_comp> && csv_string+str_pos(1).
**********************************************************************
    END-OF-DEFINITION.

    DEFINE continue_loop.
**********************************************************************
      str_pos = str_pos + 1.
      CONTINUE.
**********************************************************************
    END-OF-DEFINITION.

* ---------------------------------------------------------------------
    FREE: target_table, header_columns.

* ---------------------------------------------------------------------
    " for conv exit buffering
    GET TIME STAMP FIELD ts_parse.

* ---------------------------------------------------------------------
    string_struc = create_string_struc( target_table ).
    ASSIGN string_struc-ref->* TO <string_struc>.

* ---------------------------------------------------------------------
    str_length = strlen( csv_string ).

* ---------------------------------------------------------------------
    " first line
    append_line.

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
                append_character.
                str_pos = str_pos + 1.
                continue_loop.
              ELSE.
                delimited = abap_false.
              ENDIF.
          ENDCASE.
        WHEN separator.
          IF delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          in_cell = abap_false.
          component = component + 1.
          ASSIGN COMPONENT component OF STRUCTURE <string_struc> TO <string_comp>.
          IF sy-subrc <> 0.
            IF first_line = abap_true.
              curr_line = 1.
            ENDIF.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_many_columns
              EXPORTING
                line = curr_line.
          ENDIF.
        WHEN c_endofline_lf OR c_endofline_cr_lf(1).
          IF delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          IF  first_line = abap_true
          AND has_header = abap_true.
            fill_header_columns_tab( <string_struc> ).
            first_line = abap_false.
            FREE <string_struc>.
            IF component < string_struc-columns.
              RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                EXPORTING
                  line = 1.
            ENDIF.
            component = 1.
            ASSIGN COMPONENT component OF STRUCTURE <string_struc> TO <string_comp>.
            IF endofline = c_endofline_cr_lf.
              str_pos = str_pos + 1.
            ENDIF.
            continue_loop.
          ENDIF.
          IF (     endofline = c_endofline_cr_lf
               AND csv_string+str_pos(2) <> c_endofline_cr_lf )
          OR (     endofline = c_endofline_lf
               AND csv_string+str_pos(1) <> c_endofline_lf    )
          OR (     endofline = c_endofline_cr
               AND csv_string+str_pos(1) <> c_endofline_cr    ).
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_mixed_endofline
              EXPORTING
                line = curr_line.
          ENDIF.
          " check if rest of string is empty and parsing is finished
          CASE endofline.
            WHEN c_endofline_cr OR c_endofline_lf.
              str_pos_p1 = str_pos + 1.
            WHEN c_endofline_cr_lf ##WHEN_DOUBLE_OK.
              str_pos_p1 = str_pos + 2.
          ENDCASE.
          IF csv_string+str_pos_p1 CO space.
            IF component < string_struc-columns.
              RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                EXPORTING
                  line = curr_line.
            ENDIF.
            move_data( CHANGING source = <string_struc>
                                target = <target_line> ).
            EXIT.
            " Without && '' syntax check complains:
            " "Offsets or lengths cannot be specified for fields of type "STRING" or "XSTRING" in the current statement"
          ELSEIF contains_only_empty_lines( csv_string+str_pos_p1 && '' ).
            move_data( CHANGING source = <string_struc>
                                target = <target_line> ).
            EXIT.
          ENDIF.
          " rest of string not empty (not only spaces/endofline chars)
          IF component < string_struc-columns.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
              EXPORTING
                line = curr_line.
          ENDIF.
          move_data( CHANGING source = <string_struc>
                              target = <target_line> ).
          append_line.
          IF endofline = c_endofline_cr_lf.
            " advance position because crlf is two characters
            str_pos = str_pos + 1.
          ENDIF.
        WHEN ` `.
          IF delimited = abap_true
          OR in_cell   = abap_true.
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
          in_cell = abap_true.
          append_character.
      ENDCASE.
      IF ( str_pos + 1 ) = str_length.
        IF component < string_struc-columns.
          RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
            EXPORTING
              line = curr_line.
        ENDIF.
        move_data( CHANGING source = <string_struc>
                            target = <target_line> ).
        EXIT.
      ENDIF.
      str_pos = str_pos + 1.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_conv_exit.
* ---------------------------------------------------------------------
    CASE conv_exit.
      WHEN abap_false.
        me->conv_exit = abap_false.
      WHEN OTHERS.
        me->conv_exit = abap_true.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_delimiter.
* ---------------------------------------------------------------------
    IF delimiter = ''''
    OR delimiter = '"'.
      me->delimiter = delimiter.
    ELSE.
      RAISE EXCEPTION TYPE zcx_wd_csv_invalid_delimiter
        EXPORTING
          delimiter = delimiter.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_endofline.
* ---------------------------------------------------------------------
    " endofline can either be a linefeed/carriage return (one char)
    " or carriage return and linefeed (two chars)
    CASE endofline.
      WHEN c_endofline_lf OR c_endofline_cr_lf OR c_endofline_cr.
        me->endofline = endofline.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wd_csv_invalid_endofline
          EXPORTING
            end_of_line = endofline.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_separator.
* ---------------------------------------------------------------------
    IF  separator IS NOT INITIAL
    AND separator NA sy-abcde
    AND separator NA '0123456789'.
      me->separator = separator.
    ELSE.
      RAISE EXCEPTION TYPE zcx_wd_csv_invalid_separator
        EXPORTING
          separator = separator.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_trim_spaces.
* ---------------------------------------------------------------------
    CASE trim_spaces.
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
      ASSIGN COMPONENT sy-index OF STRUCTURE target TO FIELD-SYMBOL(<target_comp>).
      IF sy-subrc <> 0. EXIT. ENDIF.
      CONDENSE <target_comp>.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
