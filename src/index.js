import './main.css';
import { Elm } from './App.elm';
import landingImage from './Resources/landing.png';
import registerServiceWorker from './registerServiceWorker';

var storageKey = "session";

function addResources(jsonString) {
  // Parse the object
  var jsonObject = JSON.parse(jsonString);

  // Protect against a null object
  if (jsonObject == null) {
    jsonObject = { "auth": null };
  };

  // Add resources
  jsonObject["landingImage"] = landingImage;

  // Return the stringified object
  return JSON.stringify(jsonObject);
}

var app = Elm.App.init({
  node: document.getElementById('root'),
  flags: addResources(localStorage.getItem(storageKey))
});

app.ports.cache.subscribe(function (data) {
  localStorage.setItem(storageKey, JSON.stringify(data))
})

registerServiceWorker(app);
