'use strict';

require('./main.scss');

const { Elm } = require('./elm/Main.elm');
const app = Elm.Main.init({
  node: document.getElementById('main')
});

app.ports.portSetLocalStorage.subscribe( (req) => {
  localStorage.setItem(req[0],req[1]);
});

app.ports.portGetLocalStorage.subscribe( (key) => {
  const val = localStorage.getItem(key);
  app.ports.portResLocalStorage.send(val);
});

app.ports.portRemoveLocalStorage.subscribe( (key) => {
  localStorage.removeItem(key);
});
