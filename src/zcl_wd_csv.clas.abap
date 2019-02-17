CLASS zcl_wd_csv DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      mty_separator TYPE c LENGTH 1,
      mty_delimiter TYPE c LENGTH 1,
      mty_endofline TYPE string. " length can be 1 or 2 characters
    CONSTANTS:
      mc_default_separator TYPE mty_separator VALUE cl_abap_char_utilities=>horizontal_tab,
      mc_default_delimiter TYPE mty_delimiter VALUE '"',
      mc_default_endofline TYPE mty_endofline   VALUE cl_abap_char_utilities=>cr_lf.
    METHODS:
      constructor IMPORTING iv_endofline TYPE mty_endofline DEFAULT mc_default_endofline
                            iv_separator TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter TYPE mty_delimiter DEFAULT mc_default_delimiter
                  RAISING   zcx_wd_csv_invalid_endofline,
      parse_string IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                             iv_csv_string TYPE string
                   EXPORTING et_data       TYPE STANDARD TABLE
                   RAISING   cx_sy_struct_creation
                             cx_sy_conversion_error
                             zcx_wd_csv_too_many_columns
                             zcx_wd_csv_too_few_columns,
      generate_string IMPORTING iv_with_header TYPE abap_bool DEFAULT abap_false
                                it_data        TYPE STANDARD TABLE
                      EXPORTING ev_csv_string  TYPE string.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF mty_s_string_struc,
        ref     TYPE REF TO data,
        columns TYPE i,
      END OF mty_s_string_struc.
    DATA:
      mv_separator TYPE mty_separator,
      mv_delimiter TYPE mty_delimiter,
      mv_endofline TYPE mty_endofline.
    METHODS:
      create_string_struc IMPORTING it_data             TYPE ANY TABLE
                          RETURNING VALUE(rs_str_struc) TYPE mty_s_string_struc
                          RAISING   cx_sy_struct_creation,
      generate_cell IMPORTING iv_fieldname   TYPE string
                              iv_fieldtype   TYPE REF TO cl_abap_datadescr
                              iv_data        TYPE any
                    RETURNING VALUE(rv_cell) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_csv IMPLEMENTATION.


  METHOD constructor.
* ---------------------------------------------------------------------
    " endofline can either be a linefeed (one char) or carriage return and linefeed (two chars)
    CASE iv_endofline.
      WHEN cl_abap_char_utilities=>cr_lf OR cl_abap_char_utilities=>cr_lf+1(1).
        mv_endofline = iv_endofline.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_wd_csv_invalid_endofline
          EXPORTING
            end_of_line = iv_endofline.
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


  METHOD generate_cell.
* ---------------------------------------------------------------------
    DATA:
      lv_delimit TYPE abap_bool.

* ---------------------------------------------------------------------
    rv_cell = iv_data.

* ---------------------------------------------------------------------
    " escape quotes
    IF find( val = rv_cell sub = '"' ) >= 0.
      lv_delimit = abap_true.
      rv_cell = replace( val  = rv_cell
                         sub  = '"'
                         with = '""'    ).
    ENDIF.

* ---------------------------------------------------------------------
    " if the cell contains a separator or any newline character, it needs to be delimited
    IF lv_delimit = abap_false
    AND (    find( val = rv_cell sub = mv_separator                       ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf      ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf+0(1) ) >= 0
          OR find( val = rv_cell sub = cl_abap_char_utilities=>cr_lf+1(1) ) >= 0 ).
      lv_delimit = abap_true.
    ENDIF.

* ---------------------------------------------------------------------
    IF lv_delimit = abap_true.
      rv_cell = mv_delimiter && rv_cell && mv_delimiter.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD generate_string.
* ---------------------------------------------------------------------
    DATA:
      lo_tabledescr    TYPE REF TO cl_abap_tabledescr,
      lo_structdescr   TYPE REF TO cl_abap_structdescr,
      lt_components    TYPE abap_component_view_tab,
      lv_start_newline TYPE abap_bool.
    FIELD-SYMBOLS:
      <ls_component> TYPE abap_simple_componentdescr,
      <ls_data>      TYPE any,
      <lv_data>      TYPE data.

* ---------------------------------------------------------------------
    FREE ev_csv_string.

* ---------------------------------------------------------------------
    lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    lt_components = lo_structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    IF iv_with_header = abap_true.
      LOOP AT lt_components ASSIGNING <ls_component>.
        IF ev_csv_string IS INITIAL.
          ev_csv_string = generate_cell( iv_fieldname = <ls_component>-name
                                         iv_fieldtype = <ls_component>-type
                                         iv_data      = <ls_component>-name ).
        ELSE.
          ev_csv_string = ev_csv_string && mv_separator && generate_cell( iv_fieldname = <ls_component>-name
                                                                          iv_fieldtype = <ls_component>-type
                                                                          iv_data      = <ls_component>-name ).
        ENDIF.
      ENDLOOP.
      ev_csv_string = ev_csv_string && mv_endofline.
    ENDIF.

* ---------------------------------------------------------------------
    lv_start_newline = abap_true.

* ---------------------------------------------------------------------
    LOOP AT it_data ASSIGNING <ls_data>.
      LOOP AT lt_components ASSIGNING <ls_component>.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_data> TO <lv_data>.
        CASE lv_start_newline.
          WHEN abap_true.
            lv_start_newline = abap_false.
            ev_csv_string = ev_csv_string && generate_cell( iv_fieldname = <ls_component>-name
                                                            iv_fieldtype = <ls_component>-type
                                                            iv_data      = <lv_data>           ).
          WHEN abap_false.
            ev_csv_string = ev_csv_string && mv_separator && generate_cell( iv_fieldname = <ls_component>-name
                                                                            iv_fieldtype = <ls_component>-type
                                                                            iv_data      = <lv_data>           ).
        ENDCASE.
      ENDLOOP.
      ev_csv_string = ev_csv_string && mv_endofline.
      lv_start_newline = abap_true.
    ENDLOOP.

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
      ls_str_struc  TYPE mty_s_string_struc.
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

    DEFINE move_data.
**********************************************************************
      MOVE-CORRESPONDING <ls_data_str> TO <ls_data_exp>.
      FREE <ls_data_str>.
**********************************************************************
    END-OF-DEFINITION.

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
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_wd_csv_too_many_columns
              EXPORTING
                line = lv_curr_line.
          ENDIF.
        WHEN mv_endofline(1).
          IF lv_delimited = abap_true.
            append_character. continue_loop.
          ENDIF.
          CASE strlen( mv_endofline ).
            WHEN 1.
              IF lv_first_line  = abap_true
              AND iv_has_header = abap_true.
                lv_first_line = abap_false.
                continue_loop.
              ENDIF.
              IF lv_component < ls_str_struc-columns.
                RAISE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                  EXPORTING
                    line = lv_curr_line.
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
        WHEN mv_endofline+1(1).
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
          IF lv_component < ls_str_struc-columns.
            RAISE EXCEPTION TYPE zcx_wd_csv_too_few_columns
              EXPORTING
                line = lv_curr_line.
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


ENDCLASS.
