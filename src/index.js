import './main.css';
import { Elm } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

var storageKey = "session"

var app = Elm.App.init({
  node: document.getElementById('root'),
  flags: localStorage.getItem(storageKey)
});

/*
app.ports.cache.subscribe(function(data) {
  localStorage.setItem(storageKey, JSON.stringify(data))
})
*/

registerServiceWorker();
