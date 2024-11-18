*&---------------------------------------------------------------------*
*& Report zabapdde_table_structure
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapade.

" Name
" Description
" Type    Table, Structure, Append
" Appended table/structure name
" Transport request
" Package
" Table contents  Transactional, Customizing, Master data
" Maintenance via SE16(N)/SM30    SE16, SM30, No
" Edit allowed in production? Yes, No (without transport request)
"
" Search for the data elements and domains which best correspond to the criteria entered and propose them,
" they are included in the ALV Grid, there is a check mark which allows you to choose only one suggestion per column entered

CLASS lcl_app DEFINITION DEFERRED.

CONSTANTS:
  BEGIN OF c_show_hide_ddic_attr,
    hide_if_possible TYPE i VALUE 0,
    show             TYPE i VALUE 1,
    edit             TYPE i VALUE 2,
  END OF c_show_hide_ddic_attr.

TABLES dd02l.
TABLES dd02t.
TABLES dd09l.

DATA okcode                    TYPE syucomm.
DATA app                       TYPE REF TO lcl_app.
DATA is_table                  TYPE abap_bool.
DATA is_structure              TYPE abap_bool.
DATA is_transactional          TYPE abap_bool.
DATA is_customizing            TYPE abap_bool.
DATA is_master_data            TYPE abap_bool.
DATA show_hide_ddic_attr       TYPE i VALUE c_show_hide_ddic_attr-hide_if_possible.
DATA gv_suggestions_shown      TYPE abap_bool.
DATA show_hide_ddic_attr_text  TYPE c LENGTH 132.
DATA show_hide_suggestion_text TYPE c LENGTH 132.

PARAMETERS p_dd_obj TYPE dd02l-tabname MATCHCODE OBJECT dd_tabl.

START-OF-SELECTION.
  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  IF app IS NOT BOUND.
    CREATE OBJECT app TYPE ('LCL_APP').
  ENDIF.
  CALL METHOD app->('PBO').
ENDMODULE.

MODULE user_command_0100 INPUT.
  CALL METHOD app->('PAI').
ENDMODULE.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    METHODS pai.

    METHODS pbo.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_alv_table_column,
        suggestion_id TYPE i,
        "! icon_checked or icon_space
        selected      TYPE icon_l2,
        keyflag       TYPE dd03l-keyflag,
        fieldname     TYPE dd03l-fieldname,
        datatype      TYPE dd01l-datatype,
        leng          TYPE dd01l-leng,
        decimals      TYPE dd01l-decimals,
        "! Description in the Data Dictionary
        ddtext        TYPE dd03t-ddtext,
        "! Reference to a table column name instead of data element or domain name.
        ref_table     TYPE lvc_s_fcat-ref_table,
        "! Reference to a table column name instead of data element or domain name.
        ref_field     TYPE lvc_s_fcat-ref_field,
        "! Data element
        rollname      TYPE dd03l-rollname,
        "! Domain
        domname       TYPE dd03l-domname,
        "! Check table
        checktable    TYPE dd03l-checktable,
        "! Domain value table
        entitytab     TYPE dd01l-entitytab,
        convexit      TYPE dd01l-convexit,
        lowercase     TYPE dd01l-lowercase,
        outputlen     TYPE dd01l-outputlen,
        signflag      TYPE dd01l-signflag,
        outputstyle   TYPE dd01l-outputstyle,
        "! Presence of fixed values
        valexi        TYPE dd01l-valexi,
        "! NOT NULL forced
        notnull       TYPE dd03l-notnull,
        "! Column heading text
        reptext       TYPE dd04t-reptext,
        "! Maximum length for translated column heading texts
        headlen       TYPE dd04l-headlen,
        "! Short text
        scrtext_s     TYPE dd04t-scrtext_s,
        "! Maximum length for translated short texts
        scrlen1       TYPE dd04l-scrlen1,
        "! Medium text
        scrtext_m     TYPE dd04t-scrtext_m,
        "! Maximum length for translated medium texts
        scrlen2       TYPE dd04l-scrlen2,
        "! Long text
        scrtext_l     TYPE dd04t-scrtext_l,
        "! Maximum length for translated long texts
        scrlen3       TYPE dd04l-scrlen3,
        "! Field name containing the Currency code or Unit of measure
        reffield      TYPE dd03l-reffield,
        reftable      TYPE dd03l-reftable,
        "! Search help name
        shlpname      TYPE dd04l-shlpname,
        "! Search help field name
        shlpfield     TYPE dd04l-shlpfield,
        styles        TYPE lvc_t_styl,
      END OF ts_alv_table_column.
    TYPES tt_alv_table_column TYPE STANDARD TABLE OF ts_alv_table_column WITH EMPTY KEY.

    TYPES:
      BEGIN OF ts_dd02l,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
        buffered TYPE dd02l-buffered,
        mainflag TYPE dd02l-mainflag,
        contflag TYPE dd02l-contflag,
        exclass  TYPE dd02l-exclass,
      END OF ts_dd02l.
    TYPES:
      BEGIN OF ts_dd09l,
        tabname TYPE dd09l-tabname,
        tabart  TYPE dd09l-tabart,
      END OF ts_dd09l.
    TYPES:
      BEGIN OF ts_dd03l,
        tabname    TYPE dd03l-tabname,
        position   TYPE dd03l-position,
        fieldname  TYPE dd03l-fieldname,
        keyflag    TYPE dd03l-keyflag,
        rollname   TYPE dd03l-rollname,
        reftable   TYPE dd03l-reftable,
        reffield   TYPE dd03l-reffield,
        checktable TYPE dd03l-checktable,
        notnull    TYPE dd03l-notnull,
        datatype   TYPE dd03l-datatype,
        leng       TYPE dd03l-leng,
        decimals   TYPE dd03l-decimals,
      END OF ts_dd03l.
    TYPES tt_dd03l TYPE SORTED TABLE OF ts_dd03l WITH UNIQUE KEY tabname position.
    TYPES:
      BEGIN OF ts_tabname_fieldname,
        tabname   TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
      END OF ts_tabname_fieldname.
    TYPES tt_tabname_fieldname TYPE SORTED TABLE OF ts_tabname_fieldname WITH UNIQUE KEY tabname fieldname.
    TYPES:
      BEGIN OF ts_dd03t,
        fieldname TYPE dd03t-fieldname,
        ddtext    TYPE dd03t-ddtext,
      END OF ts_dd03t.
    TYPES tt_dd03t    TYPE SORTED TABLE OF ts_dd03t WITH UNIQUE KEY fieldname.
    TYPES tt_rollname TYPE SORTED TABLE OF dd04l-rollname WITH UNIQUE KEY table_line.
    TYPES:
      BEGIN OF ts_dd04l,
        rollname  TYPE dd04l-rollname,
        domname   TYPE dd04l-domname,
        shlpname  TYPE dd04l-shlpname,
        shlpfield TYPE dd04l-shlpfield,
        headlen   TYPE dd04l-headlen,
        scrlen1   TYPE dd04l-scrlen1,
        scrlen2   TYPE dd04l-scrlen2,
        scrlen3   TYPE dd04l-scrlen3,
        datatype  TYPE dd04l-datatype,
        leng      TYPE dd04l-leng,
        decimals  TYPE dd04l-decimals,
      END OF ts_dd04l.
    TYPES tt_dd04l TYPE SORTED TABLE OF ts_dd04l WITH UNIQUE KEY rollname.
    TYPES:
      BEGIN OF ts_dd04t,
        rollname  TYPE dd04t-rollname,
        ddtext    TYPE dd04t-ddtext,
        reptext   TYPE dd04t-reptext,
        scrtext_s TYPE dd04t-scrtext_s,
        scrtext_m TYPE dd04t-scrtext_m,
        scrtext_l TYPE dd04t-scrtext_l,
      END OF ts_dd04t.
    TYPES tt_dd04t   TYPE SORTED TABLE OF ts_dd04t WITH UNIQUE KEY rollname.
    TYPES tt_domname TYPE STANDARD TABLE OF dd01l-domname WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_dd01l,
        domname     TYPE dd01l-domname,
        datatype    TYPE dd01l-datatype,
        leng        TYPE dd01l-leng,
        decimals    TYPE dd01l-decimals,
        lowercase   TYPE dd01l-lowercase,
        convexit    TYPE dd01l-convexit,
        entitytab   TYPE dd01l-entitytab,
        outputlen   TYPE dd01l-outputlen,
        valexi      TYPE dd01l-valexi,
        signflag    TYPE dd01l-signflag,
        outputstyle TYPE dd01l-outputstyle,
      END OF ts_dd01l.
    TYPES tt_dd01l TYPE SORTED TABLE OF ts_dd01l WITH UNIQUE KEY domname.

    TYPES:
      BEGIN OF ts_ddic_fields,
        t_dd03l TYPE tt_dd03l,
        t_dd03t TYPE tt_dd03t,
        t_dd04l TYPE tt_dd04l,
        t_dd04t TYPE tt_dd04t,
        t_dd01l TYPE tt_dd01l,
      END OF ts_ddic_fields.

    CLASS-DATA gs_alv_client_suggestion TYPE ts_alv_table_column.
    CLASS-DATA gv_next_suggestion_id    TYPE i VALUE 1.

    DATA go_alv              TYPE REF TO cl_gui_alv_grid.
    DATA gt_alv_table_column TYPE tt_alv_table_column.
    DATA creation            TYPE abap_bool.
    DATA gs_dd02l            TYPE ts_dd02l.
    DATA gs_dd09l            TYPE ts_dd09l.
    DATA gv_dd02t_ddtext     TYPE dd02t-ddtext.
    DATA gs_ddic_fields      TYPE ts_ddic_fields.

    METHODS activate_all.

    CLASS-METHODS build_alv_table_from_ddic
      IMPORTING is_ddic_fields   TYPE ts_ddic_fields
                iv_suggestion    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rt_result) TYPE tt_alv_table_column.

    CLASS-METHODS get_lvc_fcat
      IMPORTING it_std           TYPE STANDARD TABLE
      RETURNING VALUE(rt_result) TYPE lvc_t_fcat.

    CLASS-METHODS get_table_field_metadata
      IMPORTING it_tabname_fieldname TYPE lcl_app=>tt_tabname_fieldname
      RETURNING VALUE(rs_result)     TYPE ts_ddic_fields.

    METHODS get_table_metadata.

    METHODS on_before_user_command
      FOR EVENT before_user_command
                  OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS on_button_click
      FOR EVENT button_click
                  OF cl_gui_alv_grid
      IMPORTING es_col_id es_row_no.

    METHODS on_data_changed
      FOR EVENT data_changed
                  OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    METHODS on_toolbar
      FOR EVENT toolbar
                  OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS pbo_first_time.

    METHODS save_data_elements.

    METHODS save_domains.

    METHODS save_table.

    METHODS suggest_data_elements.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD activate_all.
  ENDMETHOD.

  METHOD build_alv_table_from_ddic.
    LOOP AT is_ddic_fields-t_dd03l REFERENCE INTO DATA(ls_dd03l).
      DATA(ls_dd03t) = VALUE #( is_ddic_fields-t_dd03t[ fieldname = ls_dd03l->fieldname ] OPTIONAL ).
      IF ls_dd03l->rollname IS NOT INITIAL.
        DATA(ls_dd04l) = VALUE #( is_ddic_fields-t_dd04l[ rollname = ls_dd03l->rollname ] OPTIONAL ).
        IF ls_dd04l IS NOT INITIAL.
          DATA(ls_dd04t) = VALUE #( is_ddic_fields-t_dd04t[ rollname = ls_dd03l->rollname ] OPTIONAL ).
          DATA(ls_dd01l) = VALUE #( is_ddic_fields-t_dd01l[ domname = ls_dd04l-domname ] OPTIONAL ).
        ELSE.
          ls_dd04t = VALUE #( ).
          ls_dd01l = VALUE #( ).
        ENDIF.
      ELSE.
        ls_dd04l = VALUE #( ).
      ENDIF.
      DATA(ls_alv_table_column) = VALUE ts_alv_table_column( ).
      ls_alv_table_column-suggestion_id = COND #( WHEN iv_suggestion = abap_true THEN gv_next_suggestion_id ).
      ls_alv_table_column-keyflag       = ls_dd03l->keyflag.
      ls_alv_table_column-fieldname     = ls_dd03l->fieldname.
      ls_alv_table_column-reftable      = ls_dd03l->reftable.
      ls_alv_table_column-reffield      = ls_dd03l->reffield.
      ls_alv_table_column-checktable    = ls_dd03l->checktable.
      ls_alv_table_column-notnull       = ls_dd03l->notnull.
      ls_alv_table_column-ddtext        = ls_dd03t-ddtext.
      ls_alv_table_column-rollname      = ls_dd04l-rollname.
      ls_alv_table_column-shlpfield     = ls_dd04l-shlpfield.
      ls_alv_table_column-shlpname      = ls_dd04l-shlpname.
      ls_alv_table_column-ddtext        = ls_dd04t-ddtext.
      ls_alv_table_column-reptext       = ls_dd04t-reptext.
      ls_alv_table_column-scrtext_s     = ls_dd04t-scrtext_s.
      ls_alv_table_column-scrtext_m     = ls_dd04t-scrtext_m.
      ls_alv_table_column-scrtext_l     = ls_dd04t-scrtext_l.
      ls_alv_table_column-headlen       = ls_dd04l-headlen.
      ls_alv_table_column-scrlen1       = ls_dd04l-scrlen1.
      ls_alv_table_column-scrlen2       = ls_dd04l-scrlen2.
      ls_alv_table_column-scrlen3       = ls_dd04l-scrlen3.
      IF ls_dd01l IS NOT INITIAL.
        ls_alv_table_column-domname     = ls_dd01l-domname.
        ls_alv_table_column-datatype    = ls_dd01l-datatype.
        ls_alv_table_column-leng        = ls_dd01l-leng.
        ls_alv_table_column-decimals    = ls_dd01l-decimals.
        ls_alv_table_column-lowercase   = ls_dd01l-lowercase.
        ls_alv_table_column-convexit    = ls_dd01l-convexit.
        ls_alv_table_column-outputlen   = ls_dd01l-outputlen.
        ls_alv_table_column-signflag    = ls_dd01l-signflag.
        ls_alv_table_column-outputstyle = ls_dd01l-outputstyle.
        ls_alv_table_column-valexi      = ls_dd01l-valexi.
        ls_alv_table_column-entitytab   = ls_dd01l-entitytab.
      ELSEIF ls_dd04l IS NOT INITIAL.
        ls_alv_table_column-datatype = ls_dd04l-datatype.
        ls_alv_table_column-leng     = ls_dd04l-leng.
        ls_alv_table_column-decimals = ls_dd04l-decimals.
      ELSE.
        ls_alv_table_column-datatype = ls_dd03l->datatype.
        ls_alv_table_column-leng     = ls_dd03l->leng.
        ls_alv_table_column-decimals = ls_dd03l->decimals.
      ENDIF.

      IF iv_suggestion = abap_true.
        " Make all fields not editable.
        ls_alv_table_column-styles = VALUE #( ( style = cl_gui_alv_grid=>mc_style_disabled )
                                              ( fieldname = 'SELECTED' style = cl_gui_alv_grid=>mc_style_button ) ).
      ENDIF.

      INSERT ls_alv_table_column INTO TABLE rt_result.
    ENDLOOP.

    LOOP AT rt_result REFERENCE INTO DATA(ls_alv_line).
      ls_alv_line->selected = COND #( WHEN sy-tabix = 1 THEN icon_checked ELSE icon_space ).
    ENDLOOP.

    IF iv_suggestion = abap_true.
      gv_next_suggestion_id = gv_next_suggestion_id + 1.
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    DATA(ls_client_ddic_field) = get_table_field_metadata( VALUE #( ( tabname   = 'BNKA'
                                                                      fieldname = 'MANDT' ) ) ).
    DATA(lt_alv_client_default) = build_alv_table_from_ddic( is_ddic_fields = ls_client_ddic_field
                                                             iv_suggestion  = abap_true ).
    gs_alv_client_suggestion = VALUE #( lt_alv_client_default[ 1 ] OPTIONAL ).
    gs_alv_client_suggestion-suggestion_id = gv_next_suggestion_id.
    gv_next_suggestion_id = gv_next_suggestion_id + 1.
    " Make all fields not editable.
    gs_alv_client_suggestion-styles = VALUE #( ( style = cl_gui_alv_grid=>mc_style_disabled )
                                               ( fieldname = 'SELECTED' style = cl_gui_alv_grid=>mc_style_button ) ).
  ENDMETHOD.

  METHOD get_lvc_fcat.
    DATA lo_table   TYPE REF TO cl_salv_table.
    DATA lo_columns TYPE REF TO cl_salv_columns_list.
    DATA lo_agg     TYPE REF TO cl_salv_aggregations.
    DATA lref_t_std TYPE REF TO data.
    DATA ls_fcat    TYPE lvc_s_fcat.
    FIELD-SYMBOLS <lt_std> TYPE STANDARD TABLE.

    CREATE DATA lref_t_std LIKE it_std.
    ASSIGN lref_t_std->* TO <lt_std>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table
                                CHANGING  t_table      = <lt_std>[] ).
      CATCH cx_salv_msg.
        " TODO
    ENDTRY.

    lo_columns = lo_table->get_columns( ).
    lo_agg = lo_table->get_aggregations( ).
    rt_result = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_columns
                                                                   r_aggregations = lo_agg ).

    LOOP AT rt_result INTO ls_fcat.
      IF     ls_fcat-scrtext_l IS INITIAL
         AND ls_fcat-scrtext_m IS INITIAL
         AND ls_fcat-scrtext_s IS INITIAL
         AND ls_fcat-coltext   IS INITIAL.
        ls_fcat-scrtext_l = ls_fcat-fieldname.
        MODIFY rt_result FROM ls_fcat TRANSPORTING scrtext_l.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_metadata.
    SELECT SINGLE tabname, tabclass, buffered, mainflag, contflag, exclass
      FROM dd02l
      WHERE tabname  = @p_dd_obj
        AND as4local = 'A'
        AND as4vers  = 0
      INTO @gs_dd02l.
    IF sy-subrc <> 0.
      creation = abap_true.
      gs_dd02l = VALUE #( tabclass = 'TRANSP'
                          contflag = 'C'
                          " Extension class
                          " 0   Not classified
                          " 1   Cannot Be Enhanced
                          " 2   Can be enhanced (character-like)
                          " 3   Can be enhanced (character-like or numeric)
                          " 4   Can Be Enhanced (Deep)
                          exclass  = '0' ).
      gs_dd09l = VALUE #( tabart = 'APPL2' ).
      RETURN.
    ENDIF.

    creation = abap_false.

    SELECT SINGLE ddtext FROM dd02t
      WHERE tabname    = @p_dd_obj
        AND ddlanguage = @sy-langu
        AND as4local   = 'A'
        AND as4vers    = 0
      INTO @gv_dd02t_ddtext.

    SELECT SINGLE tabname, tabart FROM dd09l
      WHERE tabname  = @p_dd_obj
        AND as4local = 'A'
        AND as4vers  = 0
      INTO @gs_dd09l.

    DATA(lt_fieldname) = VALUE tt_tabname_fieldname( ).
    SELECT tabname, fieldname FROM dd03l
      WHERE tabname  = @p_dd_obj
        AND as4local = 'A'
        AND as4vers  = 0
        AND depth    = 0
      INTO TABLE @lt_fieldname.

    gs_ddic_fields = get_table_field_metadata( lt_fieldname ).
  ENDMETHOD.

  METHOD get_table_field_metadata.
    CHECK it_tabname_fieldname IS NOT INITIAL.

    SELECT tabname, position, fieldname, keyflag, rollname, reftable, reffield, checktable, notnull, datatype, leng,
           decimals
      FROM dd03l
      FOR ALL ENTRIES IN @it_tabname_fieldname
      WHERE tabname   = @it_tabname_fieldname-tabname
        AND fieldname = @it_tabname_fieldname-fieldname
        AND as4local  = 'A'
        AND as4vers   = 0
        AND depth     = 0
      INTO TABLE @rs_result-t_dd03l.

    SELECT fieldname, ddtext FROM dd03t
      FOR ALL ENTRIES IN @it_tabname_fieldname
      WHERE tabname    = @it_tabname_fieldname-tabname
        AND ddlanguage = @sy-langu
        AND as4local   = 'A'
        AND fieldname  = @it_tabname_fieldname-fieldname
      INTO TABLE @rs_result-t_dd03t.

    DATA(lt_rollname) = VALUE tt_rollname( ).
    LOOP AT rs_result-t_dd03l REFERENCE INTO DATA(ls_dd03l).
      INSERT ls_dd03l->rollname INTO TABLE lt_rollname.
    ENDLOOP.

    IF lt_rollname IS NOT INITIAL.
      SELECT rollname, domname, shlpname, shlpfield, headlen, scrlen1, scrlen2, scrlen3, datatype, leng, decimals
        FROM dd04l
        FOR ALL ENTRIES IN @lt_rollname
        WHERE rollname = @lt_rollname-table_line
          AND as4local = 'A'
          AND as4vers  = 0
        INTO TABLE @rs_result-t_dd04l.
    ENDIF.

    IF rs_result-t_dd04l IS NOT INITIAL.
      SELECT rollname, ddtext, reptext, scrtext_s, scrtext_m, scrtext_l
        FROM dd04t
        FOR ALL ENTRIES IN @rs_result-t_dd04l
        WHERE rollname   = @rs_result-t_dd04l-rollname
          AND ddlanguage = @sy-langu
          AND as4local   = 'A'
          AND as4vers    = 0
        INTO TABLE @rs_result-t_dd04t.
    ENDIF.

    IF rs_result-t_dd04l IS NOT INITIAL.
      DATA(lt_domname) = VALUE tt_domname( FOR <gs_dd04l> IN rs_result-t_dd04l
                                           ( <gs_dd04l>-domname ) ).
      SELECT domname, datatype, leng, decimals, lowercase, convexit, entitytab, outputlen, valexi, signflag, outputstyle
        FROM dd01l
        FOR ALL ENTRIES IN @lt_domname
        WHERE domname  = @lt_domname-table_line
          AND as4local = 'A'
          AND as4vers  = 0
        INTO TABLE @rs_result-t_dd01l.
    ENDIF.
  ENDMETHOD.

  METHOD on_before_user_command.
  ENDMETHOD.

  METHOD on_button_click.
    " |on_button_click row { es_row_no-row_id }, column { es_col_id-fieldname }| TYPE 'I'.
    CASE es_col_id-fieldname.
      WHEN 'SELECTED'.
        DATA(ls_alv_table_column) = REF #( gt_alv_table_column[ es_row_no-row_id ] OPTIONAL ).
        IF     ls_alv_table_column           IS BOUND
           AND ls_alv_table_column->selected  = icon_space.
          LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column_2)
               WHERE suggestion_id = ls_alv_table_column->suggestion_id.
            ls_alv_table_column_2->selected = icon_space.
          ENDLOOP.
          ls_alv_table_column->selected = icon_checked.
          go_alv->refresh_table_display( EXPORTING  is_stable = VALUE #( row = abap_true
                                                                         col = abap_true )
                                         EXCEPTIONS finished  = 1
                                                    OTHERS    = 2  ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_toolbar.
*    ASSIGN e_object->mt_toolbar[ function = '&LOCAL&DELETE_ROW' ] TO FIELD-SYMBOL(<fs>).
*    IF sy-subrc = 0.
*      <fs>-function = 'ZZ_DELETE_ROW'.
*    ENDIF.
  ENDMETHOD.

  METHOD pai.
    DATA(lv_okcode) = okcode.
    CLEAR okcode.

    CASE show_hide_ddic_attr.
      WHEN c_show_hide_ddic_attr-hide_if_possible
        OR c_show_hide_ddic_attr-show.

        CASE abap_true.
          WHEN is_table.
            dd02l-tabclass = 'TRANSP'.
          WHEN is_structure.
            dd02l-tabclass = 'INTTAB'.
        ENDCASE.

        CASE abap_true.
          WHEN is_transactional.
            dd02l-contflag = 'A'.
            dd09l-tabart = 'APPL1'.
          WHEN is_master_data.
            dd02l-contflag = 'A'.
            dd09l-tabart = 'APPL0'.
          WHEN is_customizing.
            dd02l-contflag = 'C'.
            dd09l-tabart = 'APPL2'.
        ENDCASE.

      WHEN c_show_hide_ddic_attr-EDIT.

*        is_table         = abap_false.
*        is_structure     = abap_false.
*        is_transactional = abap_false.
*        is_customizing   = abap_false.
*        is_master_data   = abap_false.

    ENDCASE.

    CASE lv_okcode.
      WHEN 'SAVE'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table
        DATA(gt_alv_final) = VALUE tt_alv_table_column( ).
        LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column_suggested)
             WHERE     suggestion_id <> 0
                   AND selected       = icon_checked.
          DATA(ls_alv_table_column) = REF #( gt_alv_table_column[
                                                 fieldname     = ls_alv_table_column_suggested->fieldname
                                                 suggestion_id = abap_false ]
                                             OPTIONAL ).
          IF ls_alv_table_column IS BOUND.
            DATA(ls_alv_final) = ls_alv_table_column->*.
            IF     ls_alv_final-rollname                   IS INITIAL
               AND ls_alv_table_column_suggested->rollname IS NOT INITIAL.
              ls_alv_final-rollname = ls_alv_table_column_suggested->rollname.
            ENDIF.
            INSERT ls_alv_final INTO TABLE gt_alv_final.
          ENDIF.
        ENDLOOP.

        save_domains( ).
        save_data_elements( ).
        save_table( ).
        activate_all( ).

      WHEN 'SHOW_HIDE_DDIC_ATTR'.
        " Toggle boolean
        show_hide_ddic_attr = ( show_hide_ddic_attr + 1 ) MOD 3.

      WHEN 'SHOW_HIDE_SUGGESTION'.
        gv_suggestions_shown = xsdbool( gv_suggestions_shown = abap_false ).

        DATA(lt_filter) = COND lvc_t_filt( WHEN gv_suggestions_shown = abap_false
                                           THEN VALUE #(
                                               ( fieldname = 'SUGGESTION_ID' sign = 'I' option = 'EQ' low = '0' ) ) ).
        go_alv->set_filter_criteria(
          EXPORTING
            it_filter                 = lt_filter
          EXCEPTIONS
            no_fieldcatalog_available = 1
            others                    = 2 ).
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        go_alv->refresh_table_display( EXPORTING  is_stable = VALUE #( row = abap_true
                                                                       col = abap_true )
                                       EXCEPTIONS finished  = 1
                                                  OTHERS    = 2  ).
        IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'SUGGEST'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table
        suggest_data_elements( ).

      WHEN 'QUIT'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table
        go_alv->free( ).
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    IF go_alv IS NOT BOUND.
      pbo_first_time( ).
    ENDIF.

    show_hide_ddic_attr_text = icon_view_switch && SWITCH string( show_hide_ddic_attr
                                                                  WHEN c_show_hide_ddic_attr-hide_if_possible THEN
                                                                    'Show DDIC attributes'
                                                                  WHEN c_show_hide_ddic_attr-show THEN
                                                                    'Edit DDIC attributes'
                                                                  WHEN c_show_hide_ddic_attr-edit THEN
                                                                    'Hide DDIC attributes if possible' ).
    show_hide_suggestion_text = icon_view_switch && COND string( WHEN gv_suggestions_shown = abap_true
                                                                 THEN 'Hide suggestions'
                                                                 ELSE 'Show suggestions' ).

    CASE show_hide_ddic_attr.
      WHEN c_show_hide_ddic_attr-hide_if_possible.

        is_table = abap_false.
        is_structure = abap_false.
*            IF creation = abap_true.
*              is_table = abap_true.
*            ELSE
        IF dd02l-tabclass = 'TRANSP'.
          is_table = abap_true.
        ELSEIF dd02l-tabclass = 'INTTAB'.
          is_structure = abap_true.
        ENDIF.

        is_customizing   = abap_false.
        is_transactional = abap_false.
        is_master_data   = abap_false.
*            IF creation = abap_true.
*              is_customizing = abap_true.
*            ELSE
        IF     dd02l-contflag = 'A'
           AND dd09l-tabart   = 'APPL1'.
          is_transactional = abap_true.
        ELSEIF     dd02l-contflag = 'A'
               AND dd09l-tabart   = 'APPL0'.
          is_master_data = abap_true.
        ELSEIF     dd02l-contflag = 'C'
               AND dd09l-tabart   = 'APPL2'.
          is_customizing = abap_true.
        ENDIF.

      WHEN c_show_hide_ddic_attr-edit.
        is_table         = abap_false.
        is_structure     = abap_false.
        is_transactional = abap_false.
        is_customizing   = abap_false.
        is_master_data   = abap_false.
    ENDCASE.

    SET PF-STATUS 'STATUS_0100'.

    LOOP AT SCREEN INTO DATA(screen_field).
      DATA(new_screen_field) = screen_field.
      CASE screen_field-name.
        WHEN 'DD02L-TABNAME'.
          IF creation = abap_false.
            new_screen_field-input = '0'.
          ENDIF.
        WHEN 'IS_TABLE'
          OR 'IS_STRUCTURE'.
          IF    show_hide_ddic_attr = c_show_hide_ddic_attr-edit
             OR (     is_table     = abap_false
                  AND is_structure = abap_false ).
            new_screen_field-active = '0'.
          ELSE.
            new_screen_field-active = '1'.
          ENDIF.
        WHEN 'DD02L-CONTFLAG'
          OR 'DD02L-TABCLASS'.
          IF    show_hide_ddic_attr = c_show_hide_ddic_attr-show
             OR show_hide_ddic_attr = c_show_hide_ddic_attr-edit
             OR (     is_transactional = abap_false
                  AND is_customizing   = abap_false
                  AND is_master_data   = abap_false ).
            new_screen_field-active = '1'.
            IF show_hide_ddic_attr = c_show_hide_ddic_attr-show.
              new_screen_field-input  = '0'.
            ENDIF.
          ELSE.
            new_screen_field-active = '0'.
          ENDIF.
        WHEN 'CONTENT_TYPE'
          OR 'IS_TRANSACTIONAL'
          OR 'IS_CUSTOMIZING'
          OR 'IS_MASTER_DATA'.
          IF    dd02l-tabclass = 'INTTAB'
             OR (     is_transactional = abap_false
                  AND is_customizing   = abap_false
                  AND is_master_data   = abap_false ).
            new_screen_field-active = '0'.
          ELSE.
            new_screen_field-active = '1'.
          ENDIF.
        WHEN 'DD09L-TABART'.
          IF     dd02l-tabclass <> 'INTTAB'
             AND (    show_hide_ddic_attr = c_show_hide_ddic_attr-show
                   OR show_hide_ddic_attr = c_show_hide_ddic_attr-edit
                   OR (     is_transactional = abap_false
                        AND is_customizing   = abap_false
                        AND is_master_data   = abap_false ) ).
            new_screen_field-active = '1'.
            IF show_hide_ddic_attr = c_show_hide_ddic_attr-show.
              new_screen_field-input  = '0'.
            ENDIF.
          ELSE.
            new_screen_field-active = '0'.
          ENDIF.
        WHEN 'DD02L-BUFFERED'
          OR 'DD02L-MAINFLAG'.
          IF    dd02l-tabclass      = 'INTTAB'
             OR show_hide_ddic_attr = c_show_hide_ddic_attr-hide_if_possible.
            new_screen_field-active = '0'.
          ELSE.
            new_screen_field-active = '1'.
            IF show_hide_ddic_attr = c_show_hide_ddic_attr-show.
              new_screen_field-input  = '0'.
            ENDIF.
          ENDIF.
      ENDCASE.
      IF screen_field <> new_screen_field.
        MODIFY SCREEN FROM new_screen_field.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pbo_first_time.
    get_table_metadata( ).

    dd02l-tabname = p_dd_obj.
    dd02t-ddtext = gv_dd02t_ddtext.
    dd02l-tabclass = gs_dd02l-tabclass.
    dd02l-buffered = gs_dd02l-buffered.
    dd02l-mainflag = gs_dd02l-mainflag.
    dd02l-contflag = gs_dd02l-contflag.
    dd09l-tabart = gs_dd09l-tabart.

    gt_alv_table_column = COND #( WHEN creation = abap_true AND dd02l-tabclass <> 'INTTAB'
                                  THEN VALUE #( ( keyflag = abap_true fieldname = 'MANDT' ) )
                                  ELSE build_alv_table_from_ddic( gs_ddic_fields ) ).

    DATA(go_custom) = NEW cl_gui_custom_container( container_name = 'TABLE_COLUMNS' ).
    go_alv = NEW #( i_parent = go_custom ).
    SET HANDLER on_before_user_command FOR go_alv.
    SET HANDLER on_button_click FOR go_alv.
    SET HANDLER on_data_changed FOR go_alv.
    SET HANDLER on_toolbar FOR go_alv.

    DATA(lt_fieldcatalog) = get_lvc_fcat( gt_alv_table_column ).
    TYPES:
      BEGIN OF ts_f4_field,
        fieldname TYPE lvc_s_fcat-fieldname,
        ref_table TYPE lvc_s_fcat-ref_table,
        ref_field TYPE lvc_s_fcat-ref_field,
      END OF ts_f4_field.
    TYPES tt_f4_field TYPE SORTED TABLE OF ts_f4_field WITH UNIQUE KEY fieldname.
    DATA(lt_f4_field) = VALUE tt_f4_field( ( fieldname = 'KEYFLAG  ' ref_table = 'DD03L' ref_field = 'KEYFLAG  ' )
                                           ( fieldname = 'DATATYPE ' ref_table = 'DD01L' ref_field = 'DATATYPE ' )
                                           ( fieldname = 'ROLLNAME ' ref_table = 'DD03L' ref_field = 'ROLLNAME ' )
                                           ( fieldname = 'DOMNAME  ' ref_table = 'DD03L' ref_field = 'DOMNAME  ' )
                                           ( fieldname = 'ENTITYTAB' ref_table = 'DD01L' ref_field = 'ENTITYTAB' )
                                           ( fieldname = 'NOTNULL  ' ref_table = 'DD03L' ref_field = 'NOTNULL  ' )
                                           ( fieldname = 'REFFIELD ' ref_table = 'DD03L' ref_field = 'REFFIELD ' )
                                           ( fieldname = 'REFTABLE ' ref_table = 'DD03L' ref_field = 'REFTABLE ' )
                                           ( fieldname = 'SHLPNAME ' ref_table = 'DD04L' ref_field = 'SHLPNAME ' )
                                           ( fieldname = 'LOWERCASE' ref_table = 'DD01L' ref_field = 'LOWERCASE' )
                                           ( fieldname = 'VALEXI   ' ref_table = 'DD01L' ref_field = 'VALEXI   ' ) ).
    LOOP AT lt_fieldcatalog REFERENCE INTO DATA(ls_fieldcatalog).
      CASE ls_fieldcatalog->fieldname.
        WHEN 'SUGGESTION_ID'.
          " Hide the field and the user cannot make it visible.
          ls_fieldcatalog->tech = abap_true.
        WHEN 'SELECTED'.
          ls_fieldcatalog->colddictxt = 'M'.
          ls_fieldcatalog->scrtext_m  = 'Selected'.
          ls_fieldcatalog->hotspot    = abap_true.
      ENDCASE.
      DATA(ls_f4_field) = REF #( lt_f4_field[ fieldname = ls_fieldcatalog->fieldname ] OPTIONAL ).
      IF ls_f4_field IS BOUND.
        ls_fieldcatalog->ref_table = ls_f4_field->ref_table.
        ls_fieldcatalog->ref_field = ls_f4_field->ref_field.
      ENDIF.
    ENDLOOP.

    go_alv->set_table_for_first_display( EXPORTING is_layout       = VALUE #( edit       = 'X'
                                                                              stylefname = 'STYLES'
                                                                              cwidth_opt = 'A' )
                                         CHANGING  it_outtab       = gt_alv_table_column
                                                   it_fieldcatalog = lt_fieldcatalog ).
  ENDMETHOD.

  METHOD save_data_elements.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
         WHERE     suggestion_id  = abap_false
               AND rollname   IS NOT INITIAL.

      DATA(to_name) = CONV ddobjname( ls_alv_table_column->rollname ) ##OPERATOR[DDOBJNAME].
      " state = 'A'.
      " langu = sy-langu.

*      DATA(dd01v_wa) = VALUE dd01v( ).
*      CALL FUNCTION 'DDIF_DOMA_GET'
*        EXPORTING  name          = ls_alv_table_column->domname
*        IMPORTING  dd01v_wa      = dd01v_wa    " Header of the Domain
*        EXCEPTIONS illegal_input = 1
*                   OTHERS        = 2.

      DATA(dd04v_wa) = VALUE dd04v( rollname   = ls_alv_table_column->fieldname
                                    ddlanguage = sy-langu
                                    domname    = ls_alv_table_column->domname
                                    routputlen = 0
                                    memoryid   = ''
                                    logflag    = ''
                                    headlen    = strlen( ls_alv_table_column->reptext )
                                    scrlen1    = 10
                                    scrlen2    = 20
                                    scrlen3    = 40
                                    ddtext     = ls_alv_table_column->ddtext
                                    reptext    = ls_alv_table_column->reptext
                                    scrtext_s  = ls_alv_table_column->scrtext_s
                                    scrtext_m  = ls_alv_table_column->scrtext_m
                                    scrtext_l  = ls_alv_table_column->scrtext_l
                                    actflag    = ''
                                    applclass  = '' " NON UTILISE dd01v_wa-applclass
*                                    authclass  = ls_alv_table_column->authclass ??????????
                                    as4user    = sy-uname
                                    as4date    = sy-datum
                                    as4time    = sy-uzeit
                                    dtelmaster = sy-langu
                                    reservedte = ''
                                    dtelglobal = ''
                                    shlpname   = ls_alv_table_column->shlpname
                                    shlpfield  = ls_alv_table_column->shlpfield
                                    deffdname  = ''
                                    datatype   = ls_alv_table_column->datatype
                                    leng       = ls_alv_table_column->leng
                                    decimals   = ls_alv_table_column->decimals
                                    outputlen  = ls_alv_table_column->outputlen
                                    lowercase  = ls_alv_table_column->lowercase
                                    signflag   = ls_alv_table_column->signflag
                                    convexit   = ls_alv_table_column->convexit
                                    valexi     = ls_alv_table_column->valexi
                                    entitytab  = ls_alv_table_column->entitytab
                                    refkind    = 'D'
                                    reftype    = ''
                                    proxytype  = ''
                                    ltrflddis  = ''
                                    bidictrlc  = ''
                                    nohistory  = '' ).

      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING  name              = to_name
                   dd04v_wa          = dd04v_wa
        EXCEPTIONS dtel_not_found    = 1                " No Sources for the Data Element
                   name_inconsistent = 2                " Name in Sources Inconsistent with NAME
                   dtel_inconsistent = 3                " Inconsistent Sources
                   put_failure       = 4                " Write Error (ROLLBACK Recommended)
                   put_refused       = 5                " Write not Allowed
                   OTHERS            = 6.

    ENDLOOP.
  ENDMETHOD.

  METHOD save_domains.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
         WHERE     suggestion_id  = abap_false
               AND domname    cp 'Z*'.

      DATA(to_name) = CONV ddobjname( ls_alv_table_column->domname ) ##OPERATOR[DDOBJNAME].
      " state = 'A'.
      " langu = sy-langu.

      DATA(dd01v_wa) = VALUE dd01v( ).

      dd01v_wa = VALUE dd01v( domname     = ls_alv_table_column->domname
                              ddlanguage  = sy-langu
                              datatype    = ls_alv_table_column->datatype
                              leng        = ls_alv_table_column->leng
                              outputlen   = ls_alv_table_column->outputlen
                              decimals    = ls_alv_table_column->decimals
                              lowercase   = ls_alv_table_column->lowercase
                              signflag    = ls_alv_table_column->signflag
                              langflag    = ''
                              valexi      = ''
                              entitytab   = ls_alv_table_column->entitytab
                              convexit    = ls_alv_table_column->convexit
                              mask        = ''
                              masklen     = ''
                              ddtext      = ls_alv_table_column->ddtext
                              actflag     = ''
                              applclass   = ''
                              authclass   = ''
                              as4user     = sy-uname
                              as4date     = sy-datum
                              as4time     = sy-uzeit
                              dommaster   = sy-langu
                              reservedom  = ''
                              domglobal   = ''
                              appendname  = ''
                              appexist    = ''
                              proxytype   = ''
                              outputstyle = ls_alv_table_column->outputstyle
                              ampmformat  = '' ).

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING  name              = to_name
                   dd01v_wa          = dd01v_wa
        EXCEPTIONS doma_not_found    = 1
                   name_inconsistent = 2
                   doma_inconsistent = 3
                   put_failure       = 4
                   put_refused       = 5
                   OTHERS            = 6.

    ENDLOOP.
  ENDMETHOD.

  METHOD save_table.
    DATA l_ddobjname TYPE ddobjname.
    DATA ls_dd02v    TYPE dd02v.
    DATA lt_dd03p    TYPE TABLE OF dd03p.
    DATA ls_dd03p    TYPE dd03p.

    save_domains( ).
    save_data_elements( ).

    l_ddobjname = dd02l-tabname.

    CLEAR ls_dd02v.
    ls_dd02v-tabname    = dd02l-tabname.
    ls_dd02v-ddlanguage = sy-langu.
    ls_dd02v-tabclass   = 'INTTAB'.
    ls_dd02v-ddtext     = 'Generic structure'.

    DATA l_position TYPE i.

    REFRESH lt_dd03p.
    l_position = 0.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
         WHERE suggestion_id = 0.
      l_position = l_position + 1.
      CLEAR ls_dd03p.
      ls_dd03p-tabname   = dd02l-tabname.
      ls_dd03p-position  = l_position.
      ls_dd03p-fieldname = ls_alv_table_column->fieldname.
      ls_dd03p-datatype  = ls_alv_table_column->datatype.
*    ls_dd03p-comptype  = ls_alv_table_column->comptype.
      ls_dd03p-rollname  = ls_alv_table_column->rollname.
      APPEND ls_dd03p TO lt_dd03p.
    ENDLOOP.

    l_ddobjname = dd02l-tabname.
    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING  name              = l_ddobjname
                 dd02v_wa          = ls_dd02v
      TABLES     dd03p_tab         = lt_dd03p
      EXCEPTIONS tabl_not_found    = 1
                 name_inconsistent = 2
                 tabl_inconsistent = 3
                 put_failure       = 4
                 put_refused       = 5
                 OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE 'aïe' TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD suggest_data_elements.
    DATA lt_tabname_fieldname TYPE tt_tabname_fieldname.

    DELETE gt_alv_table_column WHERE suggestion_id <> 0.

    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
        WHERE suggestion_id = 0.

      DATA(lv_tabix_after_current_row) = sy-tabix + 1.

      CASE ls_alv_table_column->fieldname.
        WHEN 'MANDT'
          OR 'CLIENT'
          OR 'MANDANT'.

          INSERT gs_alv_client_suggestion INTO gt_alv_table_column INDEX lv_tabix_after_current_row.

        WHEN OTHERS.
          IF     ls_alv_table_column->ref_table IS NOT INITIAL
             AND ls_alv_table_column->ref_field IS NOT INITIAL.
            DATA(ls_ddic_ref_field) = get_table_field_metadata(
                                          VALUE #( ( tabname   = ls_alv_table_column->ref_table
                                                     fieldname = ls_alv_table_column->ref_field ) ) ).
            DATA(lt_alv_table_column) = build_alv_table_from_ddic( is_ddic_fields = ls_ddic_ref_field
                                                                   iv_suggestion  = abap_true ).
            INSERT LINES OF lt_alv_table_column INTO gt_alv_table_column INDEX lv_tabix_after_current_row.
          ELSEIF ls_alv_table_column->rollname IS NOT INITIAL.
            lt_tabname_fieldname = VALUE tt_tabname_fieldname( ).
            SELECT dd03l~tabname,
                   dd03l~fieldname
              FROM dd03l
                     INNER JOIN
                       dd02l ON  dd02l~tabname  = dd03l~tabname
                             AND dd02l~tabclass = @dd02l-tabclass
              WHERE dd03l~rollname = @ls_alv_table_column->rollname
                AND dd03l~keyflag  = @ls_alv_table_column->keyflag
              INTO TABLE @lt_tabname_fieldname
              UP TO 10 ROWS.
            ls_ddic_ref_field = get_table_field_metadata( lt_tabname_fieldname ).
            lt_alv_table_column = build_alv_table_from_ddic( is_ddic_fields = ls_ddic_ref_field
                                                             iv_suggestion  = abap_true ).
            INSERT LINES OF lt_alv_table_column INTO gt_alv_table_column INDEX lv_tabix_after_current_row.
          ELSEIF ls_alv_table_column->fieldname IS NOT INITIAL.
            lt_tabname_fieldname = VALUE tt_tabname_fieldname( ).
            SELECT dd03l~tabname,
                   dd03l~fieldname
              FROM dd03l
                     INNER JOIN
                       dd02l ON  dd02l~tabname  = dd03l~tabname
                             AND dd02l~tabclass = @dd02l-tabclass
              WHERE dd03l~fieldname = @ls_alv_table_column->fieldname
              INTO TABLE @lt_tabname_fieldname
              UP TO 10 ROWS.
            ls_ddic_ref_field = get_table_field_metadata( lt_tabname_fieldname ).
            lt_alv_table_column = build_alv_table_from_ddic( is_ddic_fields = ls_ddic_ref_field
                                                             iv_suggestion  = abap_true ).
            INSERT LINES OF lt_alv_table_column INTO gt_alv_table_column INDEX lv_tabix_after_current_row.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    go_alv->refresh_table_display( EXPORTING  is_stable = VALUE #( row = abap_true
                                                                   col = abap_true )
                                              i_soft_refresh = abap_true
                                   EXCEPTIONS finished  = 1
                                              OTHERS    = 2  ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
