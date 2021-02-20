# gram
Gram is an executable that utilizes the Telegram Database Library & Bot API to make your life easier.

## Usage
### Prerequisites
- [Nix](https://nixos.wiki/wiki/Nix)
- A .env file with the following fields:
  - API\_ID: `App api_id` field under [My Telegram > App Configuration](https://my.telegram.org/apps)
  - API\_HASH: `App api_hash` field under [My Telegram > App Configuration](https://my.telegram.org/apps)
  - API\_PHONE\_NUMBER: This will be used for phone verification of Telegram API, enter in international format
### Running
```
user@machine:~/src/gram$ nix-build
> [2 built, 0.0 MiB DL]
user@machine:~/src/gram$ nix-shell
> gram.cabal is up-to-date
[nix-shell:~/src/gram]$ gram -c <CHAT_ID>
```

Once gram starts running you'll receive a verification code on the Telegram app, insert that into `/tmp/code`.

## Deployment
### Prerequisites
- Nixops (Install using `nix-env -i nixops`)
- Have your DigitalOcean token in env variable: export DIGITAL\_OCEAN\_AUTH\_TOKEN=\<token\>

### Create Deployment
`$ nixops create nix/trivial.nix nix/trivial-digital-ocean.nix`

`$ nixops deploy`

This will create a new Droplet in DigitalOcean & create the `gram` as a systemd service.
