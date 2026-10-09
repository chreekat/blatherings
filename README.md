# blatherings

The words behind [words.chreekat.net](https://words.chreekat.net).

## Writing

A post is a Markdown file in `posts/`, named `YYYY-MM-DD-slug.md`. It is served
at `/slug/`, so the slug is permanent once published.

```markdown
---
title: Riding the Pamir Highway
tags: cycling, trekking
description: Optional, for link previews.
updated: 2026-11-02          # optional, when revised
toc: true                    # optional table of contents
---

Words.
```

Unfinished posts go in `drafts/`, named the same way. Publishing is moving the
file to `posts/` (fixing the date if it matters) and pushing to master.

Images go in `images/` and are linked as `/images/foo.jpg`.

To preview locally, drafts included:

    nix develop
    DRAFTS=1 cabal run site -- watch

## Publishing

Every push builds the site with Nix (`nix build`, or `.#drafts`) and deploys it
to Cloudflare Pages: master is production, any other branch is a preview at
`<branch>.words-chreekat.pages.dev`.

Caching: Pages serves everything from its edge and purges it on each deploy, so
HTML is always fresh and browsers revalidate it cheaply by ETag. The stylesheet
URL carries a content hash, which lets `static/_headers` mark it immutable.

One-time setup:

1. `nix develop .#deploy --command wrangler pages project create words-chreekat --production-branch master`
2. In the Pages project, add the custom domain `words.chreekat.net`, and point a
   `CNAME` for `words` at `words-chreekat.pages.dev`.
3. Give the repository the secrets `CLOUDFLARE_ACCOUNT_ID` and
   `CLOUDFLARE_API_TOKEN` (a token scoped to *Cloudflare Pages: Edit* only).

Until the secrets exist, CI only builds and checks.

## Readership

Each page loads `/hit.js`, which, on words.chreekat.net only, fetches a
one-pixel GIF from `https://hits.chreekat.net/hit` carrying the page's path and
referrer. On puny, nginx answers from memory and appends a JSON line to
`/var/log/nginx/hits.jsonl` (`nixos/mods/hits.nix` in the dotfiles). No
addresses or cookies are kept, so there are views but no unique visitors. Readers
without JavaScript, and so most bots, go uncounted. If puny is slow or down,
pages don't notice.

To see the numbers:

    ssh puny.chreekat.net 'sudo sh -c "zcat -f /var/log/nginx/hits.jsonl*"' | nix run .#hits

One-time setup: point `hits.chreekat.net` at puny before deploying it, so ACME
can issue the certificate.

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />All aforementioned words are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
