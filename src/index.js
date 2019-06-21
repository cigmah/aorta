import './main.css';
import { Elm } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

//var storageKey = "puzzlehunt_cache"

var app = Elm.App.init({
  node: document.getElementById('root'),
  //flags: localStorage.getItem(storageKey)
});

//app.ports.storeCache.subscribe(function(data) {
//  if (data == "") {
//    localStorage.removeItem(storageKey)
//  } else {
//    localStorage.setItem(storageKey, JSON.stringify(data))
//  }
//})

registerServiceWorker();
