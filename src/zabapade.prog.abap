*&---------------------------------------------------------------------*
*& Report zabapdde_table_structure
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapade.

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
TABLES tadir.
TABLES e071.

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

CLASS ltc_app DEFINITION DEFERRED.

CLASS lcl_app DEFINITION
  FRIENDS ltc_app.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    METHODS pai.

    METHODS pbo.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_alv_table_column,
        keyflag           TYPE dd03l-keyflag,
        fieldname         TYPE dd03l-fieldname,
        datatype          TYPE dd01l-datatype,
        leng              TYPE dd01l-leng,
        decimals          TYPE dd01l-decimals,
        "! Description in the Data Dictionary
        ddtext            TYPE dd03t-ddtext,
        "! Reference to a table column name instead of data element or domain name.
        define_like_table TYPE lvc_s_fcat-ref_table,
        "! Reference to a table column name instead of data element or domain name.
        define_like_field TYPE lvc_s_fcat-ref_field,
        "! Data element
        rollname          TYPE dd03l-rollname,
        "! Domain
        domname           TYPE dd03l-domname,
        "! Check table
        checktable        TYPE dd03l-checktable,
        "! Domain value table
        entitytab         TYPE dd01l-entitytab,
        convexit          TYPE dd01l-convexit,
        lowercase         TYPE dd01l-lowercase,
        outputlen         TYPE dd01l-outputlen,
        signflag          TYPE dd01l-signflag,
        outputstyle       TYPE dd01l-outputstyle,
        "! Presence of fixed values
        valexi            TYPE dd01l-valexi,
        "! NOT NULL forced
        notnull           TYPE dd03l-notnull,
        "! Column heading text
        reptext           TYPE dd04t-reptext,
        "! Maximum length for translated column heading texts
        headlen           TYPE dd04l-headlen,
        "! Short text
        scrtext_s         TYPE dd04t-scrtext_s,
        "! Maximum length for translated short texts
        scrlen1           TYPE dd04l-scrlen1,
        "! Medium text
        scrtext_m         TYPE dd04t-scrtext_m,
        "! Maximum length for translated medium texts
        scrlen2           TYPE dd04l-scrlen2,
        "! Long text
        scrtext_l         TYPE dd04t-scrtext_l,
        "! Maximum length for translated long texts
        scrlen3           TYPE dd04l-scrlen3,
        "! Field name containing the Currency code or Unit of measure
        curr_uom_reffield TYPE dd03l-reffield,
        "! Field name containing the Currency code or Unit of measure
        curr_uom_reftable TYPE dd03l-reftable,
        "! Search help name
        shlpname          TYPE dd04l-shlpname,
        "! Search help field name
        shlpfield         TYPE dd04l-shlpfield,
        styles            TYPE lvc_t_styl,
      END OF ts_alv_table_column.
    TYPES tt_alv_table_column TYPE STANDARD TABLE OF ts_alv_table_column WITH EMPTY KEY.

    TYPES tr_string TYPE RANGE OF string.

    TYPES:
      BEGIN OF ts_result_ddif_tabl_get,
        s_dd02v TYPE dd02v,
        s_dd09l TYPE dd09v,
        t_dd03p TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY,
        t_dd05m TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY,
        t_dd08v TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
        t_dd35v TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
        t_dd36m TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY,
      END OF ts_result_ddif_tabl_get.

    TYPES tr_domname  TYPE RANGE OF domname.
    TYPES tr_rollname TYPE RANGE OF rollname.
    TYPES tr_tabname  TYPE RANGE OF tabname.

    CLASS-DATA gs_alv_client_suggestion TYPE ts_alv_table_column.
    CLASS-DATA gv_next_suggestion_id    TYPE i VALUE 1.

    DATA go_alv                    TYPE REF TO cl_gui_alv_grid.
    DATA go_alv_column_suggestions TYPE REF TO cl_gui_alv_grid.
    DATA gt_alv_table_column       TYPE tt_alv_table_column.
    DATA gt_alv_column_suggestion  TYPE tt_alv_table_column.
    DATA creation                  TYPE abap_bool.
    DATA gv_current_selection      TYPE i.
    DATA gr_domname                TYPE tr_domname.
    DATA gr_rollname               TYPE tr_rollname.
    DATA gr_tabname                TYPE tr_tabname.

    METHODS activate_all.

    CLASS-METHODS build_alv_table_from_ddic
      IMPORTING it_ddic_fields   TYPE ts_result_ddif_tabl_get-t_dd03p
                iv_suggestion    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rt_result) TYPE tt_alv_table_column.

    CLASS-METHODS get_lvc_fcat
      IMPORTING it_std           TYPE STANDARD TABLE
      RETURNING VALUE(rt_result) TYPE lvc_t_fcat.

    CLASS-METHODS to_range
      IMPORTING !input        TYPE clike
      RETURNING VALUE(result) TYPE tr_string.

    CLASS-METHODS get_table_field_metadata_2
      IMPORTING is_alv_table_column TYPE ts_alv_table_column
                iv_up_to_rows       TYPE i DEFAULT 10
      RETURNING VALUE(result)       TYPE tt_alv_table_column.

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

    METHODS on_double_click
      FOR EVENT double_click
                  OF cl_gui_alv_grid
      IMPORTING es_row_no.

    METHODS on_double_click_suggestions
      FOR EVENT double_click
                  OF cl_gui_alv_grid
      IMPORTING es_row_no.

    METHODS on_toolbar
      FOR EVENT toolbar
                  OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS on_user_command
      FOR EVENT user_command
                  OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS pbo_first_time.

    METHODS save_data_elements.

    METHODS save_domains.

    METHODS save_table.

    METHODS suggest_data_elements
      IMPORTING is_alv_table_column TYPE ts_alv_table_column
                iv_up_to_rows       TYPE i DEFAULT 10.

    METHODS tr_tadir_interface
      IMPORTING pgmid    TYPE pgmid
                !object  TYPE trobjtype
                obj_name TYPE sobj_name
                korrnum  TYPE trkorr_old
                devclass TYPE devclass.

    CLASS-METHODS ddif_tabl_get
      IMPORTING iv_objname       TYPE ddobjname
      RETURNING VALUE(rs_result) TYPE lcl_app=>ts_result_ddif_tabl_get.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD activate_all.
    gr_tabname = VALUE #( ( sign   = 'I'
                            option = 'EQ'
                            low    = dd02l-tabname ) ).
    gr_domname = VALUE #( ).
    gr_rollname = VALUE #( ).

    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column).
      IF ls_alv_table_column->domname CP 'Z*'.
        INSERT VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = ls_alv_table_column->domname )
               INTO TABLE gr_domname.
      ENDIF.
      IF ls_alv_table_column->rollname CP 'Z*'.
        INSERT VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = ls_alv_table_column->rollname )
                        INTO TABLE gr_rollname.
      ENDIF.
    ENDLOOP.
    IF    gr_tabname  IS NOT INITIAL
       OR gr_domname  IS NOT INITIAL
       OR gr_rollname IS NOT INITIAL.
      SUBMIT radmasg0
             WITH trkorr = e071-trkorr
             WITH domname  IN gr_domname
             WITH rollname IN gr_rollname
             WITH tabname  IN gr_tabname
             VIA SELECTION-SCREEN
             AND RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD build_alv_table_from_ddic.
    LOOP AT it_ddic_fields REFERENCE INTO DATA(ls_dd03l).
      DATA(ls_alv_table_column) = VALUE ts_alv_table_column( ).
      ls_alv_table_column-keyflag           = ls_dd03l->keyflag.
      ls_alv_table_column-fieldname         = ls_dd03l->fieldname.
      ls_alv_table_column-curr_uom_reftable = ls_dd03l->reftable.
      ls_alv_table_column-curr_uom_reffield = ls_dd03l->reffield.
      ls_alv_table_column-checktable        = ls_dd03l->checktable.
      ls_alv_table_column-notnull           = ls_dd03l->notnull.
      ls_alv_table_column-ddtext            = ls_dd03l->ddtext.
      ls_alv_table_column-rollname          = ls_dd03l->rollname.
      ls_alv_table_column-shlpfield         = ls_dd03l->shlpfield.
      ls_alv_table_column-shlpname          = ls_dd03l->shlpname.
      ls_alv_table_column-ddtext            = ls_dd03l->ddtext.
      ls_alv_table_column-reptext           = ls_dd03l->reptext.
      ls_alv_table_column-scrtext_s         = ls_dd03l->scrtext_s.
      ls_alv_table_column-scrtext_m         = ls_dd03l->scrtext_m.
      ls_alv_table_column-scrtext_l         = ls_dd03l->scrtext_l.
      ls_alv_table_column-headlen           = ls_dd03l->headlen.
      ls_alv_table_column-scrlen1           = ls_dd03l->scrlen1.
      ls_alv_table_column-scrlen2           = ls_dd03l->scrlen2.
      ls_alv_table_column-scrlen3           = ls_dd03l->scrlen3.
      ls_alv_table_column-domname           = ls_dd03l->domname.
      ls_alv_table_column-datatype          = ls_dd03l->datatype.
      ls_alv_table_column-leng              = ls_dd03l->leng.
      ls_alv_table_column-decimals          = ls_dd03l->decimals.
      ls_alv_table_column-lowercase         = ls_dd03l->lowercase.
      ls_alv_table_column-convexit          = ls_dd03l->convexit.
      ls_alv_table_column-outputlen         = ls_dd03l->outputlen.
      ls_alv_table_column-signflag          = ls_dd03l->signflag.
      ls_alv_table_column-outputstyle       = ls_dd03l->outputstyle.
      ls_alv_table_column-valexi            = ls_dd03l->valexi.
      ls_alv_table_column-entitytab         = ls_dd03l->entitytab.
      ls_alv_table_column-datatype          = ls_dd03l->datatype.
      ls_alv_table_column-leng              = ls_dd03l->leng.
      ls_alv_table_column-decimals          = ls_dd03l->decimals.
      ls_alv_table_column-datatype          = ls_dd03l->datatype.
      ls_alv_table_column-leng              = ls_dd03l->leng.
      ls_alv_table_column-decimals          = ls_dd03l->decimals.

      IF iv_suggestion = abap_true.
        " Make all fields not editable.
        ls_alv_table_column-styles = VALUE #( ( style = cl_gui_alv_grid=>mc_style_disabled )
                                              ( fieldname = 'SELECTED' style = cl_gui_alv_grid=>mc_style_button ) ).
      ENDIF.

      INSERT ls_alv_table_column INTO TABLE rt_result.
    ENDLOOP.

    IF iv_suggestion = abap_true.
      gv_next_suggestion_id = gv_next_suggestion_id + 1.
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    DATA(ls_bnka_metadata) = ddif_tabl_get( 'BNKA' ).
    DATA(lt_bnka_mandt_metadata) = VALUE ts_result_ddif_tabl_get-t_dd03p(
                                             ( ls_bnka_metadata-t_dd03p[ fieldname = 'MANDT' ] ) ).
    DATA(lt_alv_client_default) = build_alv_table_from_ddic( it_ddic_fields = lt_bnka_mandt_metadata
                                                             iv_suggestion  = abap_true ).
    gs_alv_client_suggestion = VALUE #( lt_alv_client_default[ 1 ] OPTIONAL ).
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

  METHOD to_range.
    DATA(string_input) = |{ input }|.
    IF string_input CA '*+'.
      result = VALUE #( ( sign = 'I' option = 'CP' low = input ) ).
    ELSEIF string_input = '='.
      result = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
    ELSEIF matches( val   = string_input
                    regex = '\d+-\d+' ).
      result = VALUE tr_string( ( sign   = 'I'
                                  option = 'BT'
                                  low    = segment( val   = string_input
                                                    sep   = '-'
                                                    index = 1 )
                                  high   = segment( val   = string_input
                                                    sep   = '-'
                                                    index = 2 ) ) ).
    ELSEIF input IS NOT INITIAL.
      result = VALUE tr_string( ( sign = 'I' option = 'EQ' low = string_input ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_table_field_metadata_2.
    TYPES tr_tabname TYPE RANGE OF tabname.

    DATA(lr_keyflag) = to_range( is_alv_table_column-keyflag ).
    DATA(lr_fieldname) = to_range( is_alv_table_column-fieldname ).
    DATA(lr_define_like_table) = to_range( is_alv_table_column-define_like_table ).
    DATA(lr_define_like_field) = to_range( is_alv_table_column-define_like_field ).
    DATA(lr_rollname) = to_range( is_alv_table_column-rollname ).
    DATA(lr_domname) = to_range( is_alv_table_column-domname ).
    DATA(lr_checktable) = to_range( is_alv_table_column-checktable ).
    DATA(lr_leng) = to_range( is_alv_table_column-leng ).
    DATA(lr_ddtext) = to_range( is_alv_table_column-ddtext ).
    DATA(lr_reptext) = to_range( is_alv_table_column-reptext ).
    DATA(lr_scrtext_s) = to_range( is_alv_table_column-scrtext_s ).
    DATA(lr_scrtext_m) = to_range( is_alv_table_column-scrtext_m ).
    DATA(lr_scrtext_l) = to_range( is_alv_table_column-scrtext_l ).

    SELECT DISTINCT dd03l~tabname                          AS define_like_table,
                    dd03l~fieldname,
                    dd03l~fieldname                        AS define_like_field,
                    dd03l~keyflag,
                    dd03l~rollname,
                    dd03l~reftable                         AS curr_uom_reftable,
                    dd03l~reffield                         AS curr_uom_reffield,
                    dd03l~checktable,
                    dd03l~notnull,
                    dd03l~datatype,
                    dd03l~leng,
                    dd03l~decimals,
                    coalesce( dd03t~ddtext, dd04t~ddtext ) AS ddtext,
                    dd04l~shlpname,
                    dd04l~shlpfield,
                    dd04l~headlen,
                    dd04l~scrlen1,
                    dd04l~scrlen2,
                    dd04l~scrlen3,
                    dd04t~reptext,
                    dd04t~scrtext_s,
                    dd04t~scrtext_m,
                    dd04t~scrtext_l,
                    dd01l~domname,
                    dd01l~lowercase,
                    dd01l~convexit,
                    dd01l~entitytab,
                    dd01l~outputlen,
                    dd01l~valexi,
                    dd01l~signflag,
                    dd01l~outputstyle
      FROM dd03l
           LEFT OUTER JOIN
           dd03t
             ON  dd03t~tabname    = dd03l~tabname
             AND dd03t~ddlanguage = @sy-langu
             AND dd03t~as4local   = 'A'
             AND dd03t~fieldname  = dd03l~fieldname
           LEFT OUTER JOIN
           dd04l
             ON  dd04l~rollname = dd03l~rollname
             AND dd04l~as4local = 'A'
             AND dd04l~as4vers  = 0
           LEFT OUTER JOIN
           dd04t
             ON  dd04t~rollname   = dd04l~rollname
             AND dd04t~ddlanguage = @sy-langu
             AND dd04t~as4local   = 'A'
             AND dd04t~as4vers    = 0
           LEFT OUTER JOIN
           dd01l
             ON  dd01l~domname  = dd03l~domname
             AND dd01l~as4local = 'A'
             AND dd01l~as4vers  = 0
      WHERE dd03l~keyflag    IN @lr_keyflag
        AND dd03l~tabname    IN @lr_define_like_table
        AND dd03l~fieldname  IN @lr_fieldname
        AND dd03l~fieldname  IN @lr_define_like_field
        AND dd03l~rollname   IN @lr_rollname
        AND dd03l~domname    IN @lr_domname
        AND dd03l~checktable IN @lr_checktable
        AND dd03l~leng       IN @lr_leng
        AND dd04t~ddtext     IN @lr_ddtext
        AND dd04t~reptext    IN @lr_reptext
        AND dd04t~scrtext_s  IN @lr_scrtext_s
        AND dd04t~scrtext_m  IN @lr_scrtext_m
        AND dd04t~scrtext_l  IN @lr_scrtext_l
        AND dd03l~as4local    = 'A'
        AND dd03l~as4vers     = 0
        AND dd03l~depth       = 0
      INTO TABLE @DATA(xxxxx)
      UP TO @iv_up_to_rows ROWS.
    result = CORRESPONDING #( xxxxx ).
  ENDMETHOD.

  METHOD on_before_user_command.
  ENDMETHOD.

  METHOD on_button_click.
    " |on_button_click row { es_row_no-row_id }, column { es_col_id-fieldname }| TYPE 'I'.
    CASE es_col_id-fieldname.
      WHEN 'SELECTED'.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_double_click.
    go_alv->check_changed_data( ). " <=== transfer screen data to internal table

    go_alv->get_current_cell( IMPORTING es_row_no = DATA(ls_row_no) ).

    gv_current_selection = ls_row_no-row_id.

    suggest_data_elements( gt_alv_table_column[ gv_current_selection ] ).

    go_alv_column_suggestions->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = abap_true
                                                                                           col = abap_true )
                                                                 i_soft_refresh = abap_true
                                                      EXCEPTIONS finished       = 1
                                                                 OTHERS         = 2  ).
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.

  METHOD on_double_click_suggestions.
    DATA(ls_alv_table_column) = REF #( gt_alv_table_column[ gv_current_selection ] ).
    DATA(lt_style) = ls_alv_table_column->styles.
    ls_alv_table_column->*      = gt_alv_column_suggestion[ es_row_no-row_id ].
    ls_alv_table_column->styles = lt_style.
    go_alv->refresh_table_display( EXPORTING  is_stable = VALUE #( row = abap_true
                                                                   col = abap_true )
                                   EXCEPTIONS finished  = 1
                                              OTHERS    = 2  ).
  ENDMETHOD.

  METHOD on_toolbar.
    INSERT VALUE #( function  = 'ZZ_DISPLAY_ALL_SUGG'
                    icon      = icon_display_more
                    butn_type = cntb_btype_button
                    text      = 'Display all suggestions' )
           INTO e_object->mt_toolbar
           INDEX 1.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'ZZ_DISPLAY_ALL_SUGG'.
        suggest_data_elements( is_alv_table_column = gt_alv_table_column[ gv_current_selection ]
                               iv_up_to_rows       = 0 ).

        go_alv_column_suggestions->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = abap_true
                                                                                               col = abap_true )
                                                                     i_soft_refresh = abap_true
                                                          EXCEPTIONS finished       = 1
                                                                     OTHERS         = 2  ).
        IF sy-subrc <> 0.
          " TODO
        ENDIF.
    ENDCASE.
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

      WHEN c_show_hide_ddic_attr-edit.

    ENDCASE.

    CASE lv_okcode.
      WHEN 'ACTIVATE'.
        activate_all( ).

      WHEN 'QUIT'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table
        go_alv->free( ).
        SET SCREEN 0.

      WHEN 'SAVE'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table
        save_domains( ).
        save_data_elements( ).
        save_table( ).
        commit work.

      WHEN 'SHOW_HIDE_DDIC_ATTR'.
        " Toggle boolean
        show_hide_ddic_attr = ( show_hide_ddic_attr + 1 ) MOD 3.

      WHEN 'SHOW_HIDE_SUGGESTION'.
        gv_suggestions_shown = xsdbool( gv_suggestions_shown = abap_false ).

        DATA(lt_filter) = COND lvc_t_filt( WHEN gv_suggestions_shown = abap_false
                                           THEN VALUE #(
                                               ( fieldname = 'SUGGESTION_ID' sign = 'I' option = 'EQ' low = '0' ) ) ).
        go_alv->set_filter_criteria( EXPORTING  it_filter                 = lt_filter
                                     EXCEPTIONS no_fieldcatalog_available = 1
                                                OTHERS                    = 2 ).
        IF sy-subrc <> 0.
          " TODO
        ENDIF.
        go_alv->refresh_table_display( EXPORTING  is_stable = VALUE #( row = abap_true
                                                                       col = abap_true )
                                       EXCEPTIONS finished  = 1
                                                  OTHERS    = 2  ).
        IF sy-subrc <> 0.
          " TODO
        ENDIF.

      WHEN 'SUGGEST'.
        go_alv->check_changed_data( ). " <=== transfer screen data to internal table

        go_alv->get_current_cell( IMPORTING es_row_no = DATA(ls_row_no) ).

        gv_current_selection = ls_row_no-row_id.

        suggest_data_elements( gt_alv_table_column[ gv_current_selection ] ).

        go_alv_column_suggestions->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = abap_true
                                                                                               col = abap_true )
                                                                     i_soft_refresh = abap_true
                                                          EXCEPTIONS finished       = 1
                                                                     OTHERS         = 2  ).
        IF sy-subrc <> 0.
          " TODO
        ENDIF.
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
        IF dd02l-tabclass = 'TRANSP'.
          is_table = abap_true.
        ELSEIF dd02l-tabclass = 'INTTAB'.
          is_structure = abap_true.
        ENDIF.

        is_customizing   = abap_false.
        is_transactional = abap_false.
        is_master_data   = abap_false.
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
              new_screen_field-input = '0'.
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
              new_screen_field-input = '0'.
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
              new_screen_field-input = '0'.
            ENDIF.
          ENDIF.
      ENDCASE.
      IF screen_field <> new_screen_field.
        MODIFY SCREEN FROM new_screen_field.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pbo_first_time.
    TYPES:
      BEGIN OF ts_f4_field,
        fieldname TYPE lvc_s_fcat-fieldname,
        ref_table TYPE lvc_s_fcat-ref_table,
        ref_field TYPE lvc_s_fcat-ref_field,
      END OF ts_f4_field.
    TYPES tt_f4_field TYPE SORTED TABLE OF ts_f4_field WITH UNIQUE KEY fieldname.

    dd02l-tabname = p_dd_obj.

    DATA(ls_result_ddif_tabl_get) = ddif_tabl_get( p_dd_obj ).

    IF ls_result_ddif_tabl_get IS INITIAL.

      creation = abap_true.

      dd02t-ddtext = ls_result_ddif_tabl_get-s_dd02v-ddtext.
      dd02l-tabclass = 'TRANSP'.
      dd02l-mainflag = 'N'. " Maintenance not allowed
      dd02l-contflag = 'C'. " Customizing
      " Extension class
      " 0   Not classified
      " 1   Cannot Be Enhanced
      " 2   Can be enhanced (character-like)
      " 3   Can be enhanced (character-like or numeric)
      " 4   Can Be Enhanced (Deep)
      dd02l-exclass  = '0'. " Customizing
      dd09l-tabart = 'APPL2'.
      dd09l-tabkat = '0'.

      show_hide_ddic_attr = c_show_hide_ddic_attr-hide_if_possible.

    ELSE.

      creation = abap_false.

      dd02t-ddtext = ls_result_ddif_tabl_get-s_dd02v-ddtext.
      dd02l-tabclass = ls_result_ddif_tabl_get-s_dd02v-tabclass.
      dd02l-buffered = ls_result_ddif_tabl_get-s_dd02v-buffered.
      dd02l-mainflag = ls_result_ddif_tabl_get-s_dd02v-mainflag.
      dd02l-contflag = ls_result_ddif_tabl_get-s_dd02v-contflag.
      dd02l-exclass  = ls_result_ddif_tabl_get-s_dd02v-exclass.
      dd09l-tabart = ls_result_ddif_tabl_get-s_dd09l-tabart.
      dd09l-tabkat = ls_result_ddif_tabl_get-s_dd09l-tabkat.

    ENDIF.

    SELECT SINGLE devclass, korrnum FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = 'TABL'
        AND obj_name = @p_dd_obj
      INTO (@tadir-devclass,@e071-trkorr).

    gt_alv_table_column = COND #( WHEN creation = abap_true AND dd02l-tabclass <> 'INTTAB'
                                  THEN VALUE #( ( keyflag = abap_true fieldname = 'MANDT' ) )
                                  ELSE build_alv_table_from_ddic( ls_result_ddif_tabl_get-t_dd03p ) ).

    DATA(lo_custom) = NEW cl_gui_custom_container( container_name = 'TABLE_COLUMNS' ).
    go_alv = NEW cl_gui_alv_grid( i_parent = lo_custom ).

    go_alv->process_ucomm_on_invalid_input( it_ucomms = VALUE #( ( CONV #( 3 ) ) ) ). " 3 = CL_GUI_ALV_GRID_BASE=>EVT_DBLCLICK_ROW_COL

    SET HANDLER on_double_click FOR go_alv.

    DATA(lt_fieldcatalog) = get_lvc_fcat( gt_alv_table_column ).
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

    lo_custom = NEW cl_gui_custom_container( container_name = 'COLUMN_SUGGESTIONS' ).
    go_alv_column_suggestions = NEW cl_gui_alv_grid( i_parent = lo_custom ).
    SET HANDLER on_double_click_suggestions FOR go_alv_column_suggestions.
    SET HANDLER on_toolbar                  FOR go_alv_column_suggestions.
    SET HANDLER on_user_command             FOR go_alv_column_suggestions.
    lt_fieldcatalog = get_lvc_fcat( gt_alv_column_suggestion ).
    go_alv_column_suggestions->set_table_for_first_display( EXPORTING is_layout       = VALUE #( cwidth_opt = 'A' )
                                                            CHANGING  it_outtab       = gt_alv_column_suggestion
                                                                      it_fieldcatalog = lt_fieldcatalog ).
  ENDMETHOD.

  METHOD save_data_elements.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
         WHERE rollname CP 'Z*'.

      DATA(lv_objname) = EXACT ddobjname( ls_alv_table_column->rollname ) ##OPERATOR[DDOBJNAME].

      DATA(dd04v_wa) = VALUE dd04v( ).
      DATA(tpara_wa) = VALUE tpara( ).
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING  name          = lv_objname
                   state         = ' '
                   langu         = sy-langu
        IMPORTING  dd04v_wa      = dd04v_wa
                   tpara_wa      = tpara_wa
        EXCEPTIONS illegal_input = 1                " Value not Allowed for Parameter
                   OTHERS        = 2.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.

      DATA(new_dd04v_wa) = VALUE dd04v( BASE dd04v_wa
                                        rollname   = ls_alv_table_column->rollname
                                        ddlanguage = sy-langu
                                        domname    = ls_alv_table_column->domname
                                        headlen    = ls_alv_table_column->headlen
                                        scrlen1    = ls_alv_table_column->scrlen1
                                        scrlen2    = ls_alv_table_column->scrlen2
                                        scrlen3    = ls_alv_table_column->scrlen3
                                        ddtext     = ls_alv_table_column->ddtext
                                        reptext    = ls_alv_table_column->reptext
                                        scrtext_s  = ls_alv_table_column->scrtext_s
                                        scrtext_m  = ls_alv_table_column->scrtext_m
                                        scrtext_l  = ls_alv_table_column->scrtext_l
                                        as4user    = sy-uname
                                        as4date    = sy-datum
                                        as4time    = sy-uzeit
                                        dtelmaster = sy-langu
                                        shlpname   = ls_alv_table_column->shlpname
                                        shlpfield  = ls_alv_table_column->shlpfield
                                        datatype   = ls_alv_table_column->datatype
                                        leng       = ls_alv_table_column->leng
                                        decimals   = ls_alv_table_column->decimals
                                        outputlen  = ls_alv_table_column->outputlen
                                        lowercase  = ls_alv_table_column->lowercase
                                        signflag   = ls_alv_table_column->signflag
                                        convexit   = ls_alv_table_column->convexit
                                        valexi     = ls_alv_table_column->valexi
                                        entitytab  = ls_alv_table_column->entitytab ).

      IF new_dd04v_wa <> dd04v_wa.
        CALL FUNCTION 'DDIF_DTEL_PUT'
          EXPORTING  name              = lv_objname
                     dd04v_wa          = new_dd04v_wa
          EXCEPTIONS dtel_not_found    = 1                " No Sources for the Data Element
                     name_inconsistent = 2                " Name in Sources Inconsistent with NAME
                     dtel_inconsistent = 3                " Inconsistent Sources
                     put_failure       = 4                " Write Error (ROLLBACK Recommended)
                     put_refused       = 5                " Write not Allowed
                     OTHERS            = 6.
        IF sy-subrc <> 0.
          " TODO
        ENDIF.

        tr_tadir_interface( pgmid    = 'R3TR'
                            object   = 'DTEL'
                            obj_name = EXACT #( lv_objname )
                            korrnum  = EXACT #( e071-trkorr )
                            devclass = tadir-devclass ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_domains.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column)
         WHERE domname CP 'Z*'.

      DATA(lv_objname) = EXACT ddobjname( ls_alv_table_column->domname ) ##OPERATOR[DDOBJNAME].

      DATA(dd01v_wa) = VALUE dd01v( ).
      TYPES tt_dd07v TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.
      DATA(dd07v_tab) = VALUE tt_dd07v( ).
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING  name          = lv_objname
        IMPORTING  dd01v_wa      = dd01v_wa    " Header of the Domain
        TABLES     dd07v_tab     = dd07v_tab
        EXCEPTIONS illegal_input = 1
                   OTHERS        = 2.
      IF sy-subrc <> 0.
        " TODO
      ENDIF.

      DATA(new_dd01v_wa) = VALUE dd01v( BASE dd01v_wa
                                        domname     = ls_alv_table_column->domname
                                        ddlanguage  = sy-langu
                                        datatype    = ls_alv_table_column->datatype
                                        leng        = ls_alv_table_column->leng
                                        outputlen   = ls_alv_table_column->outputlen
                                        decimals    = ls_alv_table_column->decimals
                                        lowercase   = ls_alv_table_column->lowercase
                                        signflag    = ls_alv_table_column->signflag
                                        entitytab   = ls_alv_table_column->entitytab
                                        convexit    = ls_alv_table_column->convexit
                                        ddtext      = ls_alv_table_column->ddtext
                                        as4user     = sy-uname
                                        as4date     = sy-datum
                                        as4time     = sy-uzeit
                                        dommaster   = sy-langu
                                        outputstyle = ls_alv_table_column->outputstyle ).

      IF new_dd01v_wa <> dd01v_wa.

        CALL FUNCTION 'DDIF_DOMA_PUT'
          EXPORTING  name              = lv_objname
                     dd01v_wa          = new_dd01v_wa
          EXCEPTIONS doma_not_found    = 1
                     name_inconsistent = 2
                     doma_inconsistent = 3
                     put_failure       = 4
                     put_refused       = 5
                     OTHERS            = 6.
        IF sy-subrc <> 0.
          " TODO
        ENDIF.

        tr_tadir_interface( pgmid    = 'R3TR'
                            object   = 'DOMA'
                            obj_name = EXACT #( lv_objname )
                            korrnum  = EXACT #( e071-trkorr )
                            devclass = tadir-devclass ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_table.
    DATA ls_dd02v    TYPE dd02v.
    DATA lt_dd03p    TYPE TABLE OF dd03p.
    DATA ls_dd03p    TYPE dd03p.

    CLEAR ls_dd02v.
    ls_dd02v-tabname    = dd02l-tabname.
    ls_dd02v-ddlanguage = sy-langu.
    ls_dd02v-tabclass   = dd02l-tabclass.
    ls_dd02v-ddtext     = dd02t-ddtext.
    ls_dd02v-exclass    = dd02l-exclass.
    ls_dd02v-contflag   = dd02l-contflag.
    ls_dd02v-mainflag   = dd02l-mainflag.

    DATA(ls_dd09l) = VALUE dd09v( tabname  = dd02l-tabname
                                  as4local = 'A'
                                  as4vers  = 0
                                  tabkat   = dd09l-tabkat
                                  tabart   = dd09l-tabart ).

    DATA(l_position) = 0.

    REFRESH lt_dd03p.
    l_position = 0.
    LOOP AT gt_alv_table_column REFERENCE INTO DATA(ls_alv_table_column).
      l_position = l_position + 1.
      CLEAR ls_dd03p.
      ls_dd03p-tabname    = dd02l-tabname.
      ls_dd03p-position   = l_position.
      ls_dd03p-keyflag    = ls_alv_table_column->keyflag.
      ls_dd03p-fieldname  = ls_alv_table_column->fieldname.
      ls_dd03p-datatype   = ls_alv_table_column->datatype.
      ls_dd03p-leng       = ls_alv_table_column->leng.
      ls_dd03p-decimals   = ls_alv_table_column->decimals.
      ls_dd03p-ddtext     = ls_alv_table_column->ddtext.
      ls_dd03p-notnull    = ls_alv_table_column->notnull.
      ls_dd03p-checktable = ls_alv_table_column->checktable.
      ls_dd03p-reftable   = ls_alv_table_column->curr_uom_reftable.
      ls_dd03p-reffield   = ls_alv_table_column->curr_uom_reffield.
      ls_dd03p-rollname   = ls_alv_table_column->rollname.
      ls_dd03p-shlpfield  = ls_alv_table_column->shlpfield.
      ls_dd03p-shlpname   = ls_alv_table_column->shlpname.
      APPEND ls_dd03p TO lt_dd03p.
    ENDLOOP.

    DATA(l_ddobjname) = exact ddobjname( dd02l-tabname ).
    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING  name              = l_ddobjname
                 dd02v_wa          = ls_dd02v
                 dd09l_wa          = ls_dd09l
      TABLES     dd03p_tab         = lt_dd03p
      EXCEPTIONS tabl_not_found    = 1
                 name_inconsistent = 2
                 tabl_inconsistent = 3
                 put_failure       = 4
                 put_refused       = 5
                 OTHERS            = 6.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.

    tr_tadir_interface( pgmid    = 'R3TR'
                        object   = 'TABL'
                        obj_name = EXACT #( l_ddobjname )
                        korrnum  = EXACT #( e071-trkorr )
                        devclass = tadir-devclass ).
  ENDMETHOD.

  METHOD suggest_data_elements.
    DATA(ls_alv_table_column) = REF #( is_alv_table_column ).
    gt_alv_column_suggestion = VALUE #( ).

    CASE ls_alv_table_column->fieldname.
      WHEN 'MANDT'
        OR 'CLIENT'
        OR 'MANDANT'.

        gt_alv_column_suggestion = VALUE #( ( gs_alv_client_suggestion ) ).

      WHEN OTHERS.
        gt_alv_column_suggestion = get_table_field_metadata_2( is_alv_table_column = is_alv_table_column
                                                               iv_up_to_rows       = iv_up_to_rows ).
    ENDCASE.
  ENDMETHOD.

  METHOD ddif_tabl_get.
    rs_result = VALUE ts_result_ddif_tabl_get( ).
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING  name          = iv_objname
                 state         = ' '
                 langu         = sy-langu
      IMPORTING  dd02v_wa      = rs_result-s_dd02v
                 dd09l_wa      = rs_result-s_dd09l
      TABLES     dd03p_tab     = rs_result-t_dd03p
                 dd05m_tab     = rs_result-t_dd05m
                 dd08v_tab     = rs_result-t_dd08v
                 dd35v_tab     = rs_result-t_dd35v
                 dd36m_tab     = rs_result-t_dd36m
      EXCEPTIONS illegal_input = 1                " Value not Allowed for Parameter
                 OTHERS        = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.

  METHOD tr_tadir_interface.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING  wi_test_modus                  = abap_false
                 wi_tadir_pgmid                 = pgmid
                 wi_tadir_object                = object
                 wi_tadir_obj_name              = obj_name
                 wi_tadir_korrnum               = korrnum
                 wi_tadir_devclass              = devclass
      EXCEPTIONS tadir_entry_not_existing       = 1
                 tadir_entry_ill_type           = 2
                 no_systemname                  = 3
                 no_systemtype                  = 4
                 original_system_conflict       = 5
                 object_reserved_for_devclass   = 6
                 object_exists_global           = 7
                 object_exists_local            = 8
                 object_is_distributed          = 9
                 obj_specification_not_unique   = 10
                 no_authorization_to_delete     = 11
                 devclass_not_existing          = 12
                 simultanious_set_remove_repair = 13
                 order_missing                  = 14
                 no_modification_of_head_syst   = 15
                 pgmid_object_not_allowed       = 16
                 masterlanguage_not_specified   = 17
                 devclass_not_specified         = 18
                 specify_owner_unique           = 19
                 loc_priv_objs_no_repair        = 20
                 gtadir_not_reached             = 21
                 object_locked_for_order        = 22
                 change_of_class_not_allowed    = 23
                 no_change_from_sap_to_tmp      = 24
                 OTHERS                         = 25.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD test.
  ENDMETHOD.
ENDCLASS.
