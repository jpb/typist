// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.appendHistory.subscribe(function(h) {
  var history = JSON.parse(window.localStorage.getItem('typistHistory') || '[]');
  history.push(h);
  window.localStorage.setItem('typistHistory', JSON.stringify(history));
});

app.ports.history.send(JSON.parse(window.localStorage.getItem('typistHistory')));
