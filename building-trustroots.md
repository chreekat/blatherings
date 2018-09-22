# Building Trustroots

After a couple hours' work, I can successfully run Trustroots locally, and I
have a small repertoire of things to try when it fails to run. I'll describe how
I got to this point, and list some possible followups.

My main source for build information was
https://github.com/Trustroots/trustroots/blob/master/INSTALL.md.
I run NixOS 17.09, a Linux distro. My preference is to run services natively,
without going through Docker or other virtualizations. But I also prefer to have
services run *local to the project*, rather than system-wide.

Trustroots relies on GraphicsMagick, Node.js/npm, and MongoDB. I used nix-shell
to make these tools available. NixOS 17.09 has a too-old version of MongoDB, so
I had nix-shell pull from a later version of Nixpkgs (the package repository
used by NixOS).

The MongoDB daemon, mongod, didn't run successfuly right off the bat. It
presumed the existence of /data/db on my filesystem. Since I want a
project-local service, I opted to run the service with the flag
`--dbpath ./.mongo-data`.

For a while, `npm start` failed due to eslint warnings. It turns out this had
something to do with a dirty worktree. My worktree was full of dust and cobwebs
from sitting around for a few years, so maybe this isn't surprising. I removed
the (ignored) files and the problem went away. I finally succeeded in running
Trustroots, with separate terminals running mongod and `npm start`.

When I tried to start Trustroots two weeks later, I ran into new problems.
Maildev errored out, saying that port 1080 was in use. (Maildev is a service
managed by npm via scripts in package.json.) I looked at system services to see
if anything else was using port 1080, but couldn't find anything.

Stymied, I tried to start up Trustroots via Docker. I had gone down this path a
few times originally, in parallel to my sometimes-blocked efforts to run
services natively. This time, I encountered new errors. It seemed the instances
were already running. Perhaps they get started automatically when I reboot my
system? After stopping the existing, running Docker instances, I was able to go
back to native services with `npm start`: Naturally, the problem with maildev
had gone away.

Now I'm pretty confident about running Trustroots. I know to keep an eye out for
two things: trash files upsetting eslint, and rogue Docker instances. I
streamlined the build docs to avoid one hangup I had, and I modified the start
script to also run mongod.

As followup, I suspect the start script will need to choose non-standard ports
for the various services so that they do not conflict with system-wide services
that devs other than myself may be separately running.

Another followup would be to add a shell.nix file to the repository, so anyone
who uses Nix (the package manager that sits at the core of NixOS and Nixpkgs)
can duplicate my setup. The nice thing about doing that is that we'd all be
using the *exact* same versions of all services! I hesitate to add the file,
however, without first discussing the strategy for keeping it up to date and
adding documentation for using it.

In the meanwhile, the entirety of this post can be summed up as:

```
nix-shell \
    -I 'nixpkgs=/home/b/.nix-defexpr/channels/unstable' \
    -p mongodb nodejs-8_x graphicsmagick \
    --run 'npm start'
```
