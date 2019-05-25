CLASS zcl_wd_csv DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      mty_separator TYPE c LENGTH 1,
      mty_delimiter TYPE c LENGTH 1,
      BEGIN OF mty_s_header_column,
        index TYPE i,
        name  TYPE string,
      END OF mty_s_header_column,
      mty_t_header_column TYPE SORTED TABLE OF mty_s_header_column WITH UNIQUE KEY index.
    CONSTANTS:
      mc_default_separator TYPE mty_separator VALUE cl_abap_char_utilities=>horizontal_tab,
      mc_default_delimiter TYPE mty_delimiter VALUE '"',
      mc_endofline_lf      TYPE c LENGTH 1    VALUE cl_abap_char_utilities=>newline,
      mc_endofline_cr_lf   TYPE c LENGTH 2    VALUE cl_abap_char_utilities=>cr_lf,
      mc_endofline_cr      TYPE c LENGTH 1    VALUE cl_abap_char_utilities=>cr_lf.
    METHODS:
      constructor IMPORTING iv_endofline   TYPE csequence     DEFAULT mc_endofline_cr_lf
                            iv_separator   TYPE mty_separator DEFAULT mc_default_separator
                            iv_delimiter   TYPE mty_delimiter DEFAULT mc_default_delimiter
                            iv_conv_exit   TYPE abap_bool     DEFAULT abap_false
                            iv_trim_spaces TYPE abap_bool     DEFAULT abap_false
                  RAISING   zcx_wd_csv_invalid_endofline
                            zcx_wd_csv_invalid_separator
                            zcx_wd_csv_invalid_delimiter,
      parse_string IMPORTING iv_has_header TYPE abap_bool DEFAULT abap_false
                             iv_csv_string TYPE string
                   EXPORTING et_data       TYPE STANDARD TABLE
                   RAISING   cx_sy_struct_creation
                             cx_sy_conversion_error
                             RESUMABLE(zcx_wd_csv_too_many_columns)
                             RESUMABLE(zcx_wd_csv_too_few_columns)
                             RESUMABLE(zcx_wd_csv_mixed_endofline),
      generate_string IMPORTING iv_with_header TYPE abap_bool DEFAULT abap_false
                                it_data        TYPE STANDARD TABLE
                      EXPORTING ev_csv_string  TYPE string,
      get_header_columns RETURNING VALUE(rt_header_columns) TYPE mty_t_header_column,
      get_separator RETURNING VALUE(rv_separator) TYPE mty_separator,
      set_separator IMPORTING iv_separator TYPE mty_separator DEFAULT mc_default_separator
                    RAISING   zcx_wd_csv_invalid_separator,
      get_endofline RETURNING VALUE(rv_endofline) TYPE string,
      set_endofline IMPORTING iv_endofline TYPE csequence DEFAULT mc_endofline_cr_lf
                    RAISING   zcx_wd_csv_invalid_endofline,
      get_delimiter RETURNING VALUE(rv_delimiter) TYPE mty_delimiter,
      set_delimiter IMPORTING iv_delimiter TYPE mty_delimiter DEFAULT mc_default_delimiter
                    RAISING   zcx_wd_csv_invalid_delimiter,
      get_conv_exit RETURNING VALUE(rv_conv_exit) TYPE abap_bool,
      set_conv_exit IMPORTING iv_conv_exit TYPE abap_bool DEFAULT abap_true,
      get_trim_spaces RETURNING VALUE(iv_trim_spaces) TYPE abap_bool,
      set_trim_spaces IMPORTING iv_trim_spaces TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF mty_s_string_struc,
        ref     TYPE REF TO data,
        columns TYPE i,
      END OF mty_s_string_struc,
      BEGIN OF mty_s_comp_convex,
        name     TYPE string,
        convexit TYPE convexit,
        temp_fld TYPE REF TO data,
      END OF mty_s_comp_convex,
      mty_t_comp_convex TYPE SORTED TABLE OF mty_s_comp_convex WITH UNIQUE KEY name.
    DATA:
      mv_endofline      TYPE string, " length can be 1 or 2 characters.
      mv_separator      TYPE mty_separator,
      mv_delimiter      TYPE mty_delimiter,
      mv_conv_exit      TYPE abap_bool,
      mv_trim_spaces    TYPE abap_bool,
      mv_ts_parse       TYPE timestampl,
      mv_ts_convex      TYPE timestampl,
      mt_comp_convex    TYPE mty_t_comp_convex,
      mt_header_columns TYPE mty_t_header_column.
    METHODS:
      create_string_struc IMPORTING it_data             TYPE ANY TABLE
                          RETURNING VALUE(rs_str_struc) TYPE mty_s_string_struc
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
                    RETURNING VALUE(rv_cell) TYPE string.
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
    IF mv_ts_convex <> mv_ts_parse.
      mv_ts_convex = mv_ts_parse.
      FREE mt_comp_convex.
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
                        temp_fld = lr                  ) INTO TABLE mt_comp_convex.
      ENDLOOP.
    ENDIF.

* ---------------------------------------------------------------------
    LOOP AT mt_comp_convex ASSIGNING FIELD-SYMBOL(<ls>).
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
    IF find( val = rv_cell sub = mv_delimiter ) >= 0.
      lv_delimit = abap_true.
      rv_cell = replace( val  = rv_cell
                         sub  = mv_delimiter
                         occ  = 0
                         with = mv_delimiter && mv_delimiter ).
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
    FREE: ev_csv_string, mt_header_columns.

* ---------------------------------------------------------------------
    lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    lt_components = lo_structdescr->get_included_view( ).

* ---------------------------------------------------------------------
    IF iv_with_header = abap_true.
      LOOP AT lt_components ASSIGNING <ls_component>.
        INSERT VALUE #( index = sy-tabix
                        name  = <ls_component>-name
                      ) INTO TABLE mt_header_columns.

        IF ev_csv_string IS INITIAL.
          ev_csv_string = generate_cell( iv_fieldname   = <ls_component>-name
                                         iv_fieldtype   = <ls_component>-type
                                         iv_data        = <ls_component>-name
                                         iv_conv_exit   = mv_conv_exit
                                         iv_trim_spaces = mv_trim_spaces       ).
        ELSE.
          ev_csv_string = ev_csv_string && mv_separator && generate_cell( iv_fieldname   = <ls_component>-name
                                                                          iv_fieldtype   = <ls_component>-type
                                                                          iv_data        = <ls_component>-name
                                                                          iv_conv_exit   = mv_conv_exit
                                                                          iv_trim_spaces = mv_trim_spaces       ).
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
            ev_csv_string = ev_csv_string && generate_cell( iv_fieldname   = <ls_component>-name
                                                            iv_fieldtype   = <ls_component>-type
                                                            iv_data        = <lv_data>
                                                            iv_conv_exit   = mv_conv_exit
                                                            iv_trim_spaces = mv_trim_spaces       ).
          WHEN abap_false.
            ev_csv_string = ev_csv_string && mv_separator && generate_cell( iv_fieldname   = <ls_component>-name
                                                                            iv_fieldtype   = <ls_component>-type
                                                                            iv_data        = <lv_data>
                                                                            iv_conv_exit   = mv_conv_exit
                                                                            iv_trim_spaces = mv_trim_spaces       ).
        ENDCASE.
      ENDLOOP.
      ev_csv_string = ev_csv_string && mv_endofline.
      lv_start_newline = abap_true.
    ENDLOOP.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_conv_exit.
* ---------------------------------------------------------------------
    rv_conv_exit = mv_conv_exit.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_delimiter.
* ---------------------------------------------------------------------
    rv_delimiter = mv_delimiter.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_endofline.
* ---------------------------------------------------------------------
    rv_endofline = mv_endofline.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD fill_header_columns_tab.
* ---------------------------------------------------------------------
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_header TO FIELD-SYMBOL(<lv>).
      IF sy-subrc <> 0. RETURN. ENDIF.
      INSERT VALUE #( index = sy-index
                      name  = <lv>
                    ) INTO TABLE mt_header_columns.
    ENDDO.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_header_columns.
* ---------------------------------------------------------------------
    rt_header_columns = mt_header_columns.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_separator.
* ---------------------------------------------------------------------
    rv_separator = mv_separator.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_trim_spaces.
* ---------------------------------------------------------------------
    iv_trim_spaces = mv_trim_spaces.

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

* ---------------------------------------------------------------------
    FREE: et_data, mt_header_columns.

* ---------------------------------------------------------------------
    " for conv exit buffering
    GET TIME STAMP FIELD mv_ts_parse.

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
              AND iv_csv_string+lv_str_pos(2) = mv_delimiter && mv_delimiter.
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
            IF lv_first_line = abap_true.
              lv_curr_line = 1.
            ENDIF.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_many_columns
              EXPORTING
                line = lv_curr_line.
          ENDIF.
        WHEN mc_endofline_lf OR mc_endofline_cr_lf(1).
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
            IF mv_endofline = mc_endofline_cr_lf.
              lv_str_pos = lv_str_pos + 1.
            ENDIF.
            continue_loop.
          ENDIF.
          IF (     mv_endofline = mc_endofline_cr_lf
               AND iv_csv_string+lv_str_pos(2) <> mc_endofline_cr_lf )
          OR (     mv_endofline = mc_endofline_lf
               AND iv_csv_string+lv_str_pos(1) <> mc_endofline_lf    )
          OR (     mv_endofline = mc_endofline_cr
               AND iv_csv_string+lv_str_pos(1) <> mc_endofline_cr    ).
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_mixed_endofline
              EXPORTING
                line = lv_curr_line.
          ENDIF.
          IF lv_component < ls_str_struc-columns.
            RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
              EXPORTING
                line = lv_curr_line.
          ENDIF.
          CASE mv_endofline.
            WHEN mc_endofline_cr OR mc_endofline_lf.
              lv_str_pos_p1 = lv_str_pos + 1.
            WHEN mc_endofline_cr_lf.
              lv_str_pos_p1 = lv_str_pos + 2.
          ENDCASE.
          IF iv_csv_string+lv_str_pos_p1 CO space.
            IF lv_component < ls_str_struc-columns.
              RAISE RESUMABLE EXCEPTION TYPE zcx_wd_csv_too_few_columns
                EXPORTING
                  line = lv_curr_line.
            ENDIF.
            move_data( EXPORTING iv_conv_exit   = mv_conv_exit
                                 iv_trim_spaces = mv_trim_spaces
                       CHANGING  cs_str = <ls_data_str>
                                 cs_exp = <ls_data_exp>          ).
            EXIT.
          ENDIF.
          move_data( EXPORTING iv_conv_exit   = mv_conv_exit
                               iv_trim_spaces = mv_trim_spaces
                     CHANGING  cs_str = <ls_data_str>
                               cs_exp = <ls_data_exp>          ).
          append_line.
          IF mv_endofline = mc_endofline_cr_lf.
            " advance position because crlf is two characters
            lv_str_pos = lv_str_pos + 1.
          ENDIF.
        WHEN ` `.
          IF lv_delimited = abap_true
          OR lv_in_cell   = abap_true.
            append_character. continue_loop.
          ELSE.
            IF mv_trim_spaces = abap_true.
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
        move_data( EXPORTING iv_conv_exit   = mv_conv_exit
                             iv_trim_spaces = mv_trim_spaces
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
        mv_conv_exit = abap_false.
      WHEN OTHERS.
        mv_conv_exit = abap_true.
    ENDCASE.

* ---------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_delimiter.
* ---------------------------------------------------------------------
    IF iv_delimiter = ''''
    OR iv_delimiter = '"'.
      mv_delimiter = iv_delimiter.
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
      WHEN mc_endofline_lf OR mc_endofline_cr_lf OR mc_endofline_cr.
        mv_endofline = iv_endofline.
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
      mv_separator = iv_separator.
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
        mv_trim_spaces = abap_false.
      WHEN OTHERS.
        mv_trim_spaces = abap_true.
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
