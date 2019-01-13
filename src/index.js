require('./assets/siimple.min.css');

const { Elm } = require('./elm/Main.elm');

Elm.Main.init({
  node: document.getElementById('main')
});
