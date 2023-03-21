# owncast-bot

a quick and dirty chat bot for owncast

## Usage

`$ ./ida-bot --help` to print command usage

`$ ./ida-bot -c your.config -p 8080` to run the bot with the specified config and on port 8080


the bot will load all lisp code in the `./commands` folder if it exists. while this is pretty unsafe it allows you (developers) to quickly create commands using the full power of lisp!


the bot comes with a few pre-built commands (check commands folder). feel free to add your own, and contribute some back to the repo if you feel like it :)

right now only chat-initiated commands are supported, but thats planning on changing soon.

## Installation

download a binary release from releases


## Author

* a. fox

## License

BSD 3-Clause

