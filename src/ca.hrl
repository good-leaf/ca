%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-include_lib("slager/src/slager.hrl").

-define(APP_NAME, ca).
-define(FALCON, application:get_env(?APP_NAME, falcon_switch, false)).
-define(NT, application:get_env(?APP_NAME, nt_switch, false)).
-define(RETRY, application:get_env(?APP_NAME, nt_retry, 3)).
-define(NT_TIMEOUT, application:get_env(?APP_NAME, nt_timeout, 1000)).

-define(NT_LEVEL(LOG_LEVEL), begin case LOG_LEVEL of error -> 1;warning -> 2;LOG_LEVEL -> skip end end).
-define(NOTICE(Context, Reason, TargetUrl, TenantId, Level),
    begin notice:create_notice_event(Context, Reason, TargetUrl, TenantId, ?NT_LEVEL(Level)) end).




%%falcon define start
-define(INFORM_INCR, 0).
-define(INFORM_CURRENT, 1).

-define(OP_ADD, add).
-define(OP_SUB, sub).
-define(OP_UPDATE, update).

-define(FALCON_REGISTER(EVENT, InformType), mt_falcon:register(EVENT, InformType)).
-define(SEND_EVENT(Event, OP, Value), mt_falcon:send_event(Event, OP, Value)).
%%falcon define end
