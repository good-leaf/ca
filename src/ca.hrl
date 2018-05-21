-include_lib("slager/src/slager.hrl").

-define(CA_FALCON, application:get_env(ca, falcon_switch, false)).
-define(NT, application:get_env(ca, nt_switch, false)).
-define(NT_RETRY, application:get_env(ca, nt_retry, 3)).
-define(NT_TIMEOUT, application:get_env(ca, nt_timeout, 1000)).

-define(NT_LEVEL(LogLevel), begin case LogLevel of error -> 1;warning -> 3 end end).
-define(CA_NOTICE(LogLevel, KvList, Message), begin notice:create_notice(LogLevel, KvList, Message) end).

-define(CA_FALCON(KvList), begin falcon:create_falcon(KvList) end).

-define(OP_ADD, add).
-define(OP_SUB, sub).
-define(OP_UPDATE, update).

-define(FALCON_REGISTER(EVENT, InformType), mt_falcon:register(EVENT, InformType)).
-define(SEND_EVENT(Event, OP, Value), mt_falcon:send_event(Event, OP, Value)).

-define(LOG(LogLevel, Format),
  begin case LogLevel of
          debug ->
            ?DEBUG(Format);
          info ->
            ?INFO(Format);
          warning ->
            ?WARNING(Format);
          error ->
            ?ERROR(Format)
        end end).

-define(LOG(LogLevel, Format, Message),
  begin case LogLevel of
          debug ->
            ?DEBUG(Format, Message);
          info ->
            ?INFO(Format, Message);
          warning ->
            ?WARNING(Format, Message);
          error ->
            ?ERROR(Format, Message)
        end end).

-define(LOG(LogLevel, Format, Message, KvList),
  begin case LogLevel of
          debug ->
            ?DEBUG(" ~p " ++ Format, [ca_log:kv_generate(KvList)] ++ Message);
          info ->
            ?INFO(" ~p " ++ Format, [ca_log:kv_generate(KvList)] ++ Message),
            ?CA_FALCON(KvList);
          warning ->
            ?WARNING(" ~p " ++ Format, [ca_log:kv_generate(KvList)] ++ Message),
            ?CA_FALCON(KvList),
            ?CA_NOTICE(LogLevel, KvList, Message);
          error ->
            ?ERROR(" ~p " ++ Format, [ca_log:kv_generate(KvList)] ++ Message),
            ?CA_FALCON(KvList),
            ?CA_NOTICE(LogLevel, KvList, Message)
        end end).

