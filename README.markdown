# ida-bot
## a chatbot for owncast servers built with alien technology

### **NOTE, THIS IS ALPHA QUALITY SOFTWARE WITH NO API DOCUMENTATION**
### **ENTER AT YOUR OWN RISK**

### Usage

`$ ./ida-bot --help` to print command usage

`$ ./ida-bot -c your.config -p 8080` to run the bot with the specified config and on port 8080

the bot loads all lisp code from `./commands`, `./handlers`, and `./services` when ran, so if you are planning on creating a custom extension please place your code in one of those folders. 

the bot comes with a few pre-built commands/handlers/services (check the appropriate folders in the repo). feel free to add your own, and contribute some back if you feel like it :)


### Installation

download a binary release from releases or see [Building](#building)

### Building 

1. install [roswell](https://github.com/roswell/roswell)
2. `$ ros install sbcl-bin/2.3.2 && ros use sbcl-bin/2.3.2`
3. `$ git clone https://github.com/compufox/simple-config ~/common-lisp/simple-config`
4. `$ git clone https://github.com/compufox/ida-bot ~/common-lisp/ida-bot`
5. `$ cd ~/common-lisp/ida-bot && make all extensions`

if all goes well, you should have a binary with all pre-built extensions in `~/common-lisp/ida-bot/bin`

### Author

- a. fox

### License

BSD 3-Clause

