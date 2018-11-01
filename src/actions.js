"use strict";

const readline = require("readline");

function make(sendResult) {
    return {
        "getArgs": () => {
            sendResult(process.argv.slice(2));
        },

        "getLine": () => { 
            const rl = readline.createInterface({
                input: process.stdin,
            });

            rl.question("", (answer) => {
                rl.close();
                sendResult(answer);
            });
        },

        "putStrLn": (line) => {
            process.stdout.write(line + "\n");
            sendResult(null);
        },
    }
};

exports['make'] = make;
