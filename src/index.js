import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { "bracket": localStorage.getItem("bracket") }
});
app.ports.dragstart.subscribe(function (event) {
  event.dataTransfer.setData("text", "");
});

app.ports.storeBracket.subscribe(function(bracket) {
  var bracketJson = JSON.stringify(bracket);
  localStorage.setItem("bracket", bracketJson);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
