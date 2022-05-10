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
 * git protocol supported: v2 (if anything)
 * record based definition of git objects (and pack encoding of these)
 * ls-refs working
 * fetch working, partially
   * can clone repos
   * can fetch repos with no change needed
   * can fetch repos with new objects

When I say "working" is that simple situations have been seen to work.

Obvious missing things:

 * Write operations (push)
 * Authentication
 * Tags
 * Delete (maybe I don't want to support this? But I probably do)
 * Shallow clones
 * Delta objects ????

### ssh

The ssh daemon will accept the "git" username, and any ssh key as
along as an ssh key is being presented. I.e, you must have a key,
doesn't matter which one. This is because I haven't focused on
this part yet.

#### About that apps/ssh directory

To be able to pass environments over ssh, I had to hack the erlang ssh
app to be able to pass the collected envs to the ssh exec fun. This is
the only way the ssh client will flag that it supports v2. (Perhaps I
can force a compatible client to use v2 by assuming it to be true? Not
tested and would probably be unwise, since we do want to support v0
clients at some point.) I do want to upstream this changes, but
it's a bit daunting, my changes to the ssh app is not super well
thought-out. Perhaps opening an issue explainng what I need could be a
start.

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

### Test

    $ rebar3 eunit

Some tests are written; some tests even test stuff of importance,
but it is undertested. As the codebase matures, the importance of
the tests will increase.

Post scriptum
-------------
I don't know why I wrote this, but I do intend to make use of it!
I think git (the tool) is well designed in how it can easily fit
in a both server and client role, and using that to build any git
service would be easy. But I thought about something stupid JFK
said once...

We have also github's own erlgitd, which even saw production use
for a short while if I understand it correctly. But it only
serves git:// (and as such is read only). And when I studied it,
I realized it only execs git (the tool). Awww, man, erlang as a
cgi shell... Wouldn't erlang exceed on all parts related to being
a git daemon? But git does a lot, so it really makes sense to rely
on it. Anyways...

So, the goal of this then? I have some reachable goals, where I
intend to use it to keep application state and sync it over ssh
between multiple nodes. Specifically, DNS zones and related
configuration. git for this purpose made sense. I've could have
(and have) implemented this directly using git (the tool) and
openssh. But now I just wanted to try my hands on doing this with
erlang.

More flufffy goals I thought about for a long time, is the
marriage of git and bittorrent related technologies. (I think you
know exactly what I mean, and you have had this thought as well.)
If this repo can be the basis for solutions in that field, I
would be thrilled.
