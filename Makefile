PROJECT = ca
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +'{parse_transform, lager_transform}' +'{lager_truncation_size, 512000}' +'{lager_extra_sinks, [sdebug, sinfo, swarning, serror]}'
DEPS = slager mt_falcon flow_falcon
dep_mt_falcon = git http://wangjun39@git.dianpingoa.com/scm/kazoo/mt_falcon.git master
dep_slager = git https://github.com/good-leaf/slager.git 1.0.0
dep_flow_falcon = git https://github.com/ccredrock/flow_falcon 1.2
include erlang.mk
