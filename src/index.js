import './main.css';
import { Elm } from './App.elm';
import landingImage from './Resources/landing.png';
import registerServiceWorker from './registerServiceWorker';

// key for session cache in localstorage
var storageKey = "session";

// hooks into and modifies jsonString to add required resources
// used when loading the initial cache object from localstorage
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

// app entry point
var app = Elm.App.init({
  node: document.getElementById('root'),
  flags: addResources(localStorage.getItem(storageKey))
});

// when the cache msg is sent, store the session data into local storage
app.ports.cache.subscribe(function (data) {
  localStorage.setItem(storageKey, JSON.stringify(data))
})

// MERMAID RENDERING
// This isn't very safe and definitely not ideal. 
// It rerenders all elements with class "land-mermaid" when called. 
// Unfortunately, it's prone to error (poor interactions with Elm's Virtal DOM?)
// Need to think if there's a more idiomatic way to do this. Otherwise, must wrap
// in a try-catch block to prevent crashing the whole app when it fails.

// external package mermaid for diagrams
var mermaid = require("mermaid");

// initialize mermaid
mermaid.mermaidAPI.initialize({
  startOnLoad: false,
  logLevel: "fatal",
});


// global variable used to auto-generate unique IDs for mermaid
// if integer overflow occurs, then reset the count and add a character to the prefix. 
// not ideal, but works for now.
var count = 0;
var prefix = "";
// generate a new unique ID prefixed with mermaid and mutate the ID template variables
function generateMermaidId() {
  var newId = "mermaid-" + prefix + String(count);
  if (count == Number.MAX_SAFE_INTEGER) {
    count = 0;
    prefix += "x";
  } else {
    count += 1;
  };
  return newId;
};

// checks for all elements with class lang-mermaid and renders mermaid
function renderMermaid() {
  var elements = document.getElementsByClassName("lang-mermaid");
  Array.prototype.forEach.call(elements, element => {
    var newId = generateMermaidId();
    try {
      var insertSvg = (svgCode) => { element.innerHTML = svgCode; element.className = "diagram-mermaid"; element.parentElement.className = "container-mermaid"; };
      var graph = mermaid.mermaidAPI.render(newId, element.textContent, insertSvg);
    } catch (e) {
      // remove the syntax error element...there should be a better way to do this i.e. not generating the error element in the first place!
      document.getElementById("d" + newId).remove();
    };
  });
};

// re-render all mermaid diagrams on page after a short delay on objective page
app.ports.rerenderMermaidObjective.subscribe(function () {
  setTimeout(() => { renderMermaid() }, 100);
})

// re-render all mermaid diagrams on page after a short delay on questions page with objective modal
app.ports.rerenderMermaidModal.subscribe(function () {
  setTimeout(() => { renderMermaid() }, 100);
})

registerServiceWorker(app);
