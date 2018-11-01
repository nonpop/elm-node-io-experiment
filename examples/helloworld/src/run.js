"use strict";

const app = require("./elm.js").Elm.Main.init();

const ioActions = require("./ioActions.js").make((data) => {
    app.ports.ioResult.send(data);
});

app.ports.ioPerform.subscribe(({ action, arg }) => {
    ioActions[action](arg);
});
