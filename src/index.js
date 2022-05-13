import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root'),
  // flags: { demoMode: false, baseUrl: "http://demo.curling.test:3000/en/admin/events/3742/brackets/", id: 1 }
  // flags: { demoMode: true, baseUrl: "http://localhost:3000/brackets/", id: 1 }
  flags: { demoMode: true, baseUrl: "http://localhost:3000/brackets/", id: 1 }
});
app.ports.dragstart.subscribe(function (event) {
  event.dataTransfer.setData("text", "");
});

app.ports.sendBracketToLocalStorage.subscribe(function(bracket) {
  var bracketJson = JSON.stringify(bracket);
  localStorage.setItem("bracket", bracketJson);
});

app.ports.requestBracketFromLocalStorage.subscribe(function() {
  var storedBracket = localStorage.getItem("bracket");
  var bracket = storedBracket ? JSON.parse(storedBracket) : null;
  app.ports.receiveBracketFromLocalStorage.send(bracket);
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
