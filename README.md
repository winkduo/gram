# gram
Gram is a service with potentially multiple features that utilize Telegram Database Library & Bot API.

# Usage:
## Prerequisites
- Haskell Tool Stack (works fine on v2.1.3)
- Nix (https://nixos.wiki/wiki/Nix)
- .env file with the following fields:
  - API\_ID: Telegram API ID (Obtainable from https://my.telegram.org)
  - API\_HASH: Telegram API ID (Obtainable from https://my.telegram.org)
  - API\_PHONE\_NUMBER: Will be used for phone verification of Telegram API

## Running
`$ stack run gram`
You will start seeing log lines like "Trying to read verification code from /tmp/code".
At this point you should have received a code on your Telegram. Type that code into /tmp/code.

# Deployment
## Prerequisites
- Nixops (Install using `nix-env -i nixops`)
- Have your DigitalOcean token in env variable: export DIGITAL\_OCEAN\_AUTH\_TOKEN=\<token\>

## Create Deployment
`$ nixops create nix/trivial.nix nix/trivial-digital-ocean.nix`
`$ nixops deploy`
This will create a new Droplet in DigitalOcean & create the `gram` as a systemd service.
