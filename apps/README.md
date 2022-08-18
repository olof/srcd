# flate
zlib library specifically made to work with the git pack
protocol. This means increased visibility of the internal
state of the zlib engine.

# ssh
A fork of OTP's ssh application. This fork contains some
work necessary to make it work with git. Namely:

* Environment handling. Needed, because it's the only way
  for the git-ssh-transport to negotiate protocol v2 features.
  OTP's ssh app throws away the envs, because setting them is
  a global state change. Which is true, only if you propagate
  it to the actual os process env. We are happy with a map
  of the client supplied envs. Probably upstreamable, somehow.
  When I get to it.
* Better handling of non-terminal clients, and binary payloads.
  Don't die as soon as you see the byte 0x3, just because it
  happen to correlate with end-of-text... Not upstreamable
  in its current form, but it really should be reported.
* Better logging. Not upstreamable. Because it's ad hoc.
