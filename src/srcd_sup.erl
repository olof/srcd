%%%-------------------------------------------------------------------
%% @doc srcd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(srcd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("srcd_object.hrl").
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      #{
        id => sshd,
        start => {srcd_ssh, start_link, []}
      },
      #{
        id => sample_empty_repo,
        start => {srcd_repo, start_link, ["/empty.git"]}
      },
      #{
        id => test_empty_repo,
        start => {srcd_repo, start_link, ["/pushtest.git"]}
      },
      #{
        id => sample_repo_2,
        start => {srcd_repo, start_link, [
          "/no-content.git",
          [
            {"refs/heads/master", "4a5ddb3241c127daa27cf1ba74adba1f284f6693"}
          ],
          [
            #object{id="4a5ddb3241c127daa27cf1ba74adba1f284f6693",
                    data=#commit{
                      tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
                      parents=["4bd8bdbdc6661187350f6e6141577c3d7cda1ac6"],
                      author=#stamp{name="Olof Johansson",
                                    email="olof@ethup.se",
                                    time=1651498600,
                                    tz="+0200"},
                      committer=#stamp{name="Olof Johansson",
                                       email="olof@ethup.se",
                                       time=1651498600,
                                       tz="+0200"},
                      msg="second\n"
                  }},
            #object{id="4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
                    data=#commit{
                      tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
                      author=#stamp{name="Olof Johansson",
                                    email="olof@ethup.se",
                                    time=1651430945,
                                    tz="+0200"},
                      committer=#stamp{name="Olof Johansson",
                                       email="olof@ethup.se",
                                       time=1651430945,
                                       tz="+0200"},
                      msg="test\n"
                    }},
            #object{id="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
	            data=#tree{items=[]}}
          ]
        ]}
      },
      #{
        id => sample_repo_3,
        start => {srcd_repo, start_link, [
          "/content.git",
          [
            {"refs/heads/master", "2000f31abf7f7fb344a9e9f4ad3e396f1b8fe46a"}
          ],
          [
            #object{
              id="2000f31abf7f7fb344a9e9f4ad3e396f1b8fe46a",
              data=#commit{
                tree="bf39464be82b4c0c6f26551a9ae5905fe80747c8",
                parents=["4a5ddb3241c127daa27cf1ba74adba1f284f6693"],
                author=#stamp{name="Olof Johansson",
                              email="olof@ethup.se",
                              time=1651519667,
                              tz="+0200"},
                committer=#stamp{name="Olof Johansson",
                                 email="olof@ethup.se",
                                 time=1651519667,
                                 tz="+0200"},
                msg="first data\n"
              }
            },
            #object{id="bf39464be82b4c0c6f26551a9ae5905fe80747c8",
                    data=#tree{items=[
                      #tree_node{
                        mode="100644", name="test",
                        object="1269488f7fb1f4b56a8c0e5eb48cecbfadfa9219"
                      }
                    ]}},
            #object{id="1269488f7fb1f4b56a8c0e5eb48cecbfadfa9219",
                    data=#blob{data="data\n"}},
            #object{id="4a5ddb3241c127daa27cf1ba74adba1f284f6693",
                    data=#commit{
                      tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
                      parents=["4bd8bdbdc6661187350f6e6141577c3d7cda1ac6"],
                      author=#stamp{name="Olof Johansson",
                                    email="olof@ethup.se",
                                    time=1651498600,
                                    tz="+0200"},
                      committer=#stamp{name="Olof Johansson",
                                       email="olof@ethup.se",
                                       time=1651498600,
                                       tz="+0200"},
                      msg="second\n"
                    }},
            #object{id="4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
                    data=#commit{
                      tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
                      author=#stamp{name="Olof Johansson",
                                    email="olof@ethup.se",
                                    time=1651430945,
                                    tz="+0200"},
                      committer=#stamp{name="Olof Johansson",
                                       email="olof@ethup.se",
                                       time=1651430945,
                                       tz="+0200"},
                      msg="test\n"
                    }},
            #object{id="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
                    data=#tree{items=[]}}
          ]
        ]}
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
