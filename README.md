srcd
====

**(Work in progress)** srcd: a git compatible ssh daemon.

Also note: I *will* rebase the history before making it public.
Things like removing "changes" commits and removing my personal
information from sample repos :). Oh, yeah, I aim to make it
public and open source.

This repo does not represent a usable git daemon yet. But some
things work:

 * transport protocols supported: ssh
 * git protocol supported: a little bit of v0, a little bit of v2
 * record based definition of git objects (and pack encoding of these)
 * packfile and object parsing
 * ls-refs working
 * fetch working, partially
   * can clone repos
   * can fetch repos with no change needed
   * can fetch repos with new objects
 * push working, partially
   * only simples object types

When I say "working" is that simple situations have been seen to work.

Obvious missing things:

 * Persistance of changes (write to disk; survive restarts)
   * Currently, only hardcoded, in-memory repos are supported.
   * sqlite, mnesia, .git directory structure
   * just act as a git proxy, persist it in a remote repo?
 * Authentication (ssh key manager / tie in to SSO)
 * Required object types:
   * Tags
   * `ref_delta` objects
 * Optional protocol [capabilities][git-caps]:
   * `ofs_delta` objects
   * Thin packs
   * Delete references
   * Shallow clones
   * status-reports

[git-caps]: https://github.com/git/git/blob/master/Documentation/technical/protocol-capabilities.txt

### Things i may want to do with this.
I don't know why I wrote this, but I do intend to make use of it!
I think git (the tool) is well designed in how it can easily fit
in a both server and client role, and using that to build any git
service would be easy.

When it comes to git and erlang, we have also github's own
[egtid][egitd], which even saw production use for a short while
if I understand it correctly. But it only serves git:// (and as
such is read only). And when I studied it, I realized it only
execs git (the tool). Awww, man, erlang as a cgi shell...
Wouldn't erlang exceed on all parts related to being a git
daemon? But git does a lot, so it really makes sense to rely on
it. Anyways...

So, the goal of this then? I have some reachable goals, where I
intend to use it to keep application state and sync it over ssh
between multiple nodes. Specifically, DNS zones and related
configuration. git for this purpose made sense. I've could have
(and have) implemented this directly using git (the tool) and
openssh. But now I just wanted to try my hands on doing this with
erlang.

As part of this group of use cases, I want to integrate some kind
of rendering/build support, so that clients can ask for content
over http. We'll also want hook support, so that we can trigger
these builds (and more, like updating DNS zonedata) on push.
These things (except for hooks) may not necessarily be part of
this repo directly.

[erlgitd]: https://github.com/mojombo/egitd

### ssh

The ssh daemon will accept the "git" username, and any ssh key as
along as an ssh key is being presented. I.e, you must have a key,
doesn't matter which one. This is because I haven't focused on
this part yet.

#### About that apps/ssh directory

I've had to fork erlang's ssh implementation during development
of srcd for several reasons:

* To be able to pass environments over ssh, I had to hack the
  erlang ssh app to be able to pass the collected envs to the ssh
  exec fun. This is the only way the ssh client will flag that it
  supports v2. (Perhaps I can force a compatible client to use v2
  by assuming it to be true? Not tested and would probably be
  unwise, since we do want to support v0 clients at some point.)
  I do want to upstream this changes, but it's a bit daunting, my
  changes to the ssh app is not super well thought-out. Perhaps
  opening an issue explainng what I need could be a start.

* Byte value 3 anywhere in a stream is interpreted as `^C`; the
  git pack protocol is binary and will by necessity sometime
  encode a three. I removed this handling entirely; that's not a
  change I would accept as-is if I were an erlang maintainer, but
  maybe gives a hint that the `^C` handling probably belongs
  somewhere else (or at least be configurable).

* Improve server side error logging a little bit; when the
  connection is killed, the error is sent on stderr *to the
  client*. It is not logged on the server at all. At least
  `LOG_NOTICE` when it happens (I still don't have the traceback,
  so I sometime have to guess what "undef" or "function_clause"
  means). I also really don't want this info sent to the client,
  but that's still happening for now.

These changes are limited to the `ssh_cli` and `ssh_options`
modules, but because of how erlang releases work, only
incorporating these modules in my application will cause a
conflict. This is why I have vendored the whole ssh app.

Hacking
-------

Currently, my workflow is just to use

    $ HOST=localhost PORT=22222 rebar3 shell

This will start an ssh daemon on localhost port 22222 that will
accept any ssh key and the "git" user. (Yes, this is one of the
things that makes it **not production ready**.)

It serves three sample repos, defined in `srcd_sup.erl`:

 * `/empty.git`: a repo without any objects at all
 * `/no-content.git`: a repo with only commits and empty trees,
                      no blobs
 * `/content.git`: a repo with commits, trees and blobs

You can thus clone one of these using something like

    git clone ssh://git@localhost:22222/content.git

Note that you must present a ssh key, even though it doesn't
matter which one.

Git objects are stored in memory (a map, in `srcd_repo`) as
records. Have a look at `srcd_sup` for how to define repos and
objects. The records are defined in `srcd_objects.hrl`.  In its
current state, since we don't support push operations, the only
way to define new objects it to carefully handcraft them - or,
easier: generate them using git and carefully copy the values
(including any trailing newlines of blob data).

A trivial blob object could be defined (for ingestion by
`srcd_repo`) as:

    {"1269488f7fb1f4b56a8c0e5eb48cecbfadfa9219", #blob{data="data\n"}}

### Git tips

To inspect the content of a compressed object, I've found

    pigz -zd <objects/ff/ffffff....

to be useful, since it can directly work with deflate compressed files.

When developing git protocol stuff, git can be helpful in providing
traces of what it sees and does:

    GIT_TRACE_PACKET=1 git clone ...

And if that doesn't help, because your issues are within the
packfile itself, you can have git dump it for you:

    GIT_TRACE_PACKFILE=$PWD/packfile.bin git clone ...

You can remove the header by doing

    tail -c +15 packfile.bin >packfile.payload

You can do `pigz -zd <packfile.payload`, but that will only get
you the first object (sometimes enough!). Perhaps I'll develop a
small perl script to split such files as a development aide.

Update: Or you can create a new empty repo and do `git
unpack-objects <packfile.bin`. That may also work. Might even be
simpler.

### Test

    $ rebar3 eunit      # erlang based unit tests
    $ ./tests/run.sh    # shell script based integration tests, using git

Some tests are written; some tests even test stuff of importance,
but it is undertested. As the codebase matures, the importance of
the tests will increase.
